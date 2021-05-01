function formatCommas(nStr, dp) {
  var x = nStr.toFixed(dp).split('.');
  x[1] = x.length > 1 ? '.' + x[1] : '';
  var rgx = /(\d+)(\d{3})/;
    while (rgx.test(x[0])) {
      x[0] = x[0].replace(rgx, '$1' + ',' + '$2');
    }
    return x[0] + x[1];  
}

function formatWcaTime(x) {
  if(x < 60) {
    return x.toFixed(2)+' seconds';
  } else if(x % 60 < 10){
    return (Math.floor(x/60))+':0'+(x % 60).toFixed(2);
  } else {
    return (Math.floor(x/60))+':'+(x % 60).toFixed(2);
  }
}

function formatWcaMbd(x) {
  var dd = Math.floor(x/10000000);
  var mm = x % 100;
  var t = Math.floor(x / 100) % 100000;
  if(t == 99999) {
  return dd+' net solves ('+(dd+mm)+'/'+(dd+2*mm)+')';
  } else {
  var m = Math.floor(t / 60);
  var s = t % 60;
  return dd+' net solves ('+(dd+mm)+'/'+(dd+2*mm)+' '+(m < 10 ? '0' : '')+m+':'+(s < 10 ? '0' : '')+s+')';
  }
}

function formatWcaMove(x) {
  if(Math.floor(x) == x) {
    return x+' moves';
  } else {
    return x.toFixed(2)+' moves';
  }
}