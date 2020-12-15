const EventEmitter = require('events').EventEmitter;

const emit = new EventEmitter();
emit.setMaxListeners(50);
export {emit};
