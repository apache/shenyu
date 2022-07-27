package org.apache.shenyu.common.concurrent;

/**
 * A handler for rejected element that discards the oldest element.
 */
public class DiscardOldestPolicy<E> implements Rejector<E> {

    @Override
    public void reject(final E e, final MemorySafeLinkedBlockingQueue<E> queue) {
        queue.poll();
        queue.offer(e);
    }
}