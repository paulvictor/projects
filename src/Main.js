const readline = require('readline');

exports._rlOnlyReadable = function(readable){
  return function(){
    return readline.createInterface({
      input: readable,
      terminal: false
    });
  }
}

exports._getLine = function(rl){
  return function(c){
    return function(err, succ){
      rl.on('line', succ)
      return function(ce, cancErr, cancSucc){
        c();
        cancSucc();
      }
    }
  }
}
