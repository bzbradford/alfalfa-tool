const WI_BOUNDS = {
  south: 42.4, north: 47.1,
  west: -93.0, east: -86.8,
}

const MW_BOUNDS = {
  south: 38.0, north: 49.4,
  west: -98.0, east: -82.0,
}

// callback for google location searchbox
function initAutocomplete() {
  const searchbox = document.getElementById('map-searchbox');
  const opts = {
    types: ['geocode'],
    bounds: WI_BOUNDS,
    strictBounds: true,
  }
  autocomplete = new google.maps.places.Autocomplete(searchbox, opts);

  autocomplete.setFields(['geometry']);
  autocomplete.addListener('place_changed', function() {
    const place = autocomplete.getPlace();
    if (!place.geometry) return;
    const loc = place.geometry.location;
    const coords = { lat: loc.lat(), lng: loc.lng() }
    Shiny.setInputValue('map-user_loc', coords, { priority: 'event' });
  });
}
