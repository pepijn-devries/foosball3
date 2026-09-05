Shiny.addCustomMessageHandler('scroll-next-foosball-news', function(message) {
  var el = document.getElementById(message.text_id);
  if (!el) return;
  
  el.getAnimations().forEach(function(anim) { anim.cancel(); });
  el.innerHTML = message.text;
  
  var animation = el.animate([
    { transform: 'translateX(100vw)' },
    { transform: 'translateX(-100%)' }
  ], {
    duration: message.duration,
    iterations: 1,
    easing: 'cubic-bezier(0.06, 0.63, 0.94, 0.37)',
    fill: 'forwards'
  });
  
  animation.onfinish = function() {
    Shiny.setInputValue(message.mod_id + '-animation_finished', Date.now());
  };
});

let foosball_timer_id = null;
let foosball_timer_start = 0;
const foostball_timer_precount = 3000;
const zeroPad = (num, places) => String(num).padStart(places, '0');

Shiny.addCustomMessageHandler('foosball_timer', function(message) {
  var aud_precount = $('#foosball-aud-precount').get(0);
  var aud_midmatch = $('#foosball-aud-midmatch').get(0);
  var aud_countdown = $('#foosball-aud-countdown').get(0);

  var show_time = function(tim) {
    var el = document.getElementById(message.timer_id);
    if (el) {
      var sec = tim % 60;
      var min = Math.floor(tim/60);
      if (min > 99) min = 99;
      if (tim == Math.floor(message.milliseconds/2000)) {
        if (aud_precount) {
          aud_midmatch.play();
        }
      }
      if (tim > Math.round(message.milliseconds/1000)) {
        var prec = tim - Math.round(message.milliseconds/1000);
        el.className = "foosball-timer-precount"
        el.innerHTML = "\u221200:" + String(prec).padStart(2, '0');
      } else if (tim > 0) {
        if (tim > 5) {
          el.className = "foosball-timer"
        } else {
          if (tim == 5 && aud_countdown) {
            aud_countdown.play();
          }
          el.className = "foosball-timer-danger"
        }
        el.innerHTML = "\u2007" + String(min).padStart(2, '0') + ":" + String(sec).padStart(2, '0');
      } else {
        el.innerHTML = "\u200700:00";
      }
    }
  }
  if (message.operator === 'start' && foosball_timer_id === null) {
    foosball_timer_start = new Date().getTime() + message.milliseconds + foostball_timer_precount;
    var el = document.getElementById(message.timer_id);
    if (el) {
      el.className = "foosball-timer-precount"
    }
    if (aud_precount) {
      aud_precount.play();
    }

    show_time(Math.round((message.milliseconds + foostball_timer_precount)/1000));
    foosball_timer_id = setInterval(() => {
      var el = document.getElementById(message.timer_id);
      var tick = Math.round((foosball_timer_start - new Date().getTime())/1000);
      show_time(tick);
      if (tick <= 0) {
        clearInterval(foosball_timer_id);
        foosball_timer_id = null;
      }
    }, 1000);
  } else if (message.operator === 'stop' && foosball_timer_id !== null) {
    clearInterval(foosball_timer_id);
    foosball_timer_id = null;
    var el = document.getElementById(message.timer_id);
    if (el) {
      show_time(Math.round(message.milliseconds/1000));
    }
    if (aud_precount) {
      aud_precount.pause();
      aud_precount.currentTime = 0;
      aud_countdown.pause();
      aud_countdown.currentTime = 0;
    }

  };
});