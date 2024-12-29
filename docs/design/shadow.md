# Shadow layouts

## Background

Nimbus does not have control over when applications launch or create windows. This creates challenges in a number of situations:

* At startup, Nimbus must connect to every individual application to register and control its windows. Applications may respond at different speeds or be completely unresponsive. We still want to do something not just reasonable, but intelligent and predictable, with the user's windows.
* When an application relaunches after an update, we want the restored windows on a space to be handled correctly.
* When the machine is rebooted, we want to restore the old layouts and slot reopened windows into them as they're discovered.

The challenge these situations, particularly the latter two, all have in common is that they must happen asynchronously. During startup we could reasonably wait until all applications have been registered (or a timeout occurs), but when applications relaunch, we have very little control or visibility into when their windows will reopen. Setting an arbitrary timeout would create a bad user experience. We need to do something reasonable while we wait for the windows to reappear, and continue doing something reasonable whether or not they ever reappear.

## What is a shadow layout?

The solution for these situations is called *shadow layouts*. A shadow layout is a copy of a real layout with one node for every node in the original layout, plus additional nodes for windows that may get added to the layout in the future.

Shadow layouts act exactly like real layouts for the most part, except they are not used to control actual windows on screen. Every action that occurs on a real layout is mirrored on the shadow layout. This means that many actions accept not a singular `NodeId` but a `NodePair`. A `NodePair` consists of two optional ids, one for the actual node and one for its shadow.

When a window for a shadow node is registered, that shadow is reified into an actual node. This creates an event that is handled by every component. In simple cases the component data is copied from the shadow into the reified node; in others the operation may be more complex.

Conversely, when an application terminates, its actual window nodes are all removed, but the shadow nodes are kept.

## Tree structure

A particular challenge that occurs is that the tree structure of shadow layout might diverge from the actual layout.

This is most commonly a result of tree "rebalancing" actions that delete unnecessary nodes. These actions can be triggered on the actual layout and subsequently on the shadow layout, but they might not take place on the shadow layout if it has more nodes than the actual layout. The shadow layout must always preserve the invariant that every node in the actual layout has a corresponding shadow node.

When rebalancing only occus in the actual layout, the operation must take care not to mirror any reparenting actions in the shadow layout. This can be accomplished with the `NodePair::actual_only()` method.

When reifying a node, we must ensure that its parent is also reified in the actual layout. We can do this recursively until we reach a parent which has already been reified – the root is guaranteed to be reified.

Note however that this can result in surprising behavior: We might reify a lineage of nested container nodes with one child each. In most cases, nodes like this do not appear in the actual layout, and they owe their existence to shadow nodes that are not otherwise observable by the user. In practice I do not anticipate this being a major issue, because windows are usually restored together. If users want window restoration, they will have to be willing to make this tradeoff.

An alternative might be to only reify containers who will have more than one child node in the actual layout tree. This would mean that for skipped containers, when a second child was reified, the existing child would be removed from its parent in the actual layout and moved underneath the newly reified parent. I don't see a reason why this can't work, but it requires some bookkeeping and a fair amount of testing. (How would we handle resizes when the nodes have different parents/siblings in the shadow tree? I guess we could pick the shadow ancestor that is a child of the actual node's parent and resize that in the shadow tree instead of the actual node. Here we can think of the actual node as "standing in" for the whole subtree that contains it and will be restored later.)

## Cleaning up shadow nodes

This only becomes manageable with some strategy for cleaning up nodes.

The simplest one is to keep shadow nodes until the app is launched and terminated again without seeing the window corresponding to that node. After a full reboot we could extend this to two launches to handle the case where the app is relaunched and the user chooses to relaunch a second time to update it before restoring their windows.

The more "invisible" we make unrestored nodes, the more conservative we can be about deleting them, but they do have to get cleaned up at some point.

## Impl notes

* Structure change operations are mostly on `DetachedNode`; we can make this store a `NodePair` internally.

## Open questions

### Detecting relaunches

Can we detect when an application is relaunching and avoid removing all of its windows from the layout? Probably not. Even windows closed by the user can be reopened later. If we had to we could make a timeout that decided that if the app is still running the window isn't coming back, and do any rebalancing then. Hopefully we can avoid that.

Observed behaviors:

* Zed: Application terminates without WindowDestroyed events. Application launches with no windows, then we get a WindowCreated event for the restored window with the correct title.
* Chrome: We get WindowDestroyed events before application terminates. Application launches with no windows, then we get a WindowCreated event for every restored window (mostly with correct titles but some like "extension asdf was updated"). This includes windows from other spaces, which are then moved to another space (silently; we have no way of detecting this at the moment).
  * Maybe the windows are opened on other spaces even though we get events for them; we could detect this with NSWindow.windowNumbers or CGWindowList APIs.
  * I think Chrome is important enough that it's worth keeping shadow windows around for, but we need to be able to clean them up when they are never getting restored.
