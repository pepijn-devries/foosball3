let absoluteRotationDegrees = 0;

Shiny.addCustomMessageHandler('animate_coin', function(data) {
  const coin = document.getElementById(data.coinId);
  const audio = document.getElementById(data.audioId);
  const btn = document.getElementById(data.btnId);
  
  if (!coin) return;
  if (btn) btn.disabled = true;

  function executeSpinAnimation(durationSeconds) {
    const cinematicSpins = 2160;
    const landingOffset = data.outcome === 'Heads' ? 0 : 180;
    
    absoluteRotationDegrees += cinematicSpins + (landingOffset - (absoluteRotationDegrees % 360));

    coin.style.transition = `transform ${durationSeconds}s cubic-bezier(0.15, 0.85, 0.25, 1)`;
    coin.style.transform = `rotateY(${absoluteRotationDegrees}deg)`;

    setTimeout(() => {
      if (btn) btn.disabled = false;
      
      if (audio) {
        audio.pause();
        audio.currentTime = 0;
      }
      
      Shiny.setInputValue('my_flip_module-animation_complete', data.outcome, {priority: 'event'});
    }, durationSeconds * 1000);
  }

  if (audio) {
    audio.currentTime = 0;
    
    if (audio.duration && !isNaN(audio.duration) && audio.duration > 0) {
      audio.play().catch(e => console.warn("Autoplay block tracker: ", e));
      executeSpinAnimation(audio.duration + 0.5);
    } else {
      audio.addEventListener('loadedmetadata', function onMetadataLoad() {
        audio.play().catch(e => console.warn("Autoplay block tracker: ", e));
        executeSpinAnimation(audio.duration);
        
        audio.removeEventListener('loadedmetadata', onMetadataLoad);
      });
      
      audio.load();
    }
  } else {
    executeSpinAnimation(3.5);
  }
});
