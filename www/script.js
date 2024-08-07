const BOUNDS = {
  north: 47.1,
  south: 42.4,
  east: -86.8,
  west: -93.0
}

// callback for google location searchbox
function initAutocomplete() {
  const searchbox = document.getElementById('map-searchbox');
  const opts = {
    types: ['geocode'],
    bounds: BOUNDS,
    strictBounds: true,
  }
  const autocomplete = new google.maps.places.Autocomplete(searchbox, opts);

  autocomplete.setFields(['geometry']);
  autocomplete.addListener('place_changed', function() {
    const place = autocomplete.getPlace();
    if (!place.geometry) return;
    const loc = place.geometry.location;
    const coords = { lat: loc.lat(), lng: loc.lng() }
    Shiny.setInputValue('map-user_loc', coords, { priority: 'event' });
  });
}
