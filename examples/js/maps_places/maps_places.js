var map;
var infowindow;
let current_marker;

let lat;
let lng;

let my_loc;

var loc_options = {
    // enableHighAccuracy: true,
    timeout: 5000,
    maximumAge: 0
};

function createMarker () {
    var myloc = new google.maps.Marker({
        clickable: false,
        icon: {
            path: google.maps.SymbolPath.FORWARD_CLOSED_ARROW,
            strokeColor : '#3333FF',
            strokeWeight : 5,
            scale: 2.5
        },
        shadow: null,
        zIndex: 999,
        map
    });
    return myloc;
}

function position_first (position) {
    lat = position.coords.latitude;
    lng = position.coords.longitude;

    map.setCenter({lat, lng});
    myloc.setPosition({lat, lng});
}

function refresh_current (position) {
    lat = position.coords.latitude;
    lng = position.coords.longitude;

    myloc.setPosition({lat, lng});
    map.setCenter({lat, lng});

    console.log(position.coords.latitude + " ",
                position.coords.longitude  + " ",
                position.coords.accuracy);



}

function refresh_error (err) {
    console.log("Error on location");
}


function enableOrientationArrow() {

    if (window.DeviceOrientationEvent) {

        window.addEventListener('deviceorientation', function(event) {
            var alpha = null;
            //Check for iOS property
            if (event.webkitCompassHeading) {
                alpha = event.webkitCompassHeading;
            }
            //non iOS
            else {
                alpha = event.alpha;
            }
            var locationIcon = myloc.get('icon');
            locationIcon.rotation = 360 - alpha;
            myloc.set('icon', locationIcon);
        }, false);
    }
}

function initMap () {
    map = new google.maps.Map(document.getElementById('map'), {
        zoom: 15
    });

    infowindow = new google.maps.InfoWindow();
    myloc = createMarker ();

    navigator.geolocation.getCurrentPosition(position_first, refresh_error, loc_options);
    navigator.geolocation.watchPosition(refresh_current, refresh_error, loc_options);

    init ();
    enableOrientationArrow();

}

function add_marker_to_map(place) {
    var placeLoc = place.geometry.location;
    var marker = new google.maps.Marker({
        map: map,
        position: place.geometry.location
    });

    google.maps.event.addListener(marker, 'click', function() {
        infowindow.setContent(place.name);
        infowindow.open(map, this);
    });
}

function init() {

    var searchBox = new google.maps.places.SearchBox(document.getElementById('pac-input'));
    map.controls[google.maps.ControlPosition.TOP_CENTER].push(document.getElementById('pac-input'));
    google.maps.event.addListener(searchBox, 'places_changed', function() {
        searchBox.set('map', null);


        var places = searchBox.getPlaces();

        var bounds = new google.maps.LatLngBounds();
        var i, place;
        for (i = 0; place = places[i]; i++) {
            (function(place) {
                var marker = new google.maps.Marker({
                    position: place.geometry.location
                });
                marker.bindTo('map', searchBox, 'map');
                google.maps.event.addListener(marker, 'map_changed', function() {
                    if (!this.getMap()) {
                        this.unbindAll();
                    }
                });
                bounds.extend(place.geometry.location);


            }(place));

        }
        map.fitBounds(bounds);
        searchBox.set('map', map);
        map.setZoom(Math.min(map.getZoom(),26));

    });
}
// google.maps.event.addDomListener(window, 'load', init);
