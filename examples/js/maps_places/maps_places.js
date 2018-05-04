var map;
var infowindow;
var locate_btn;
var lat, lng;
var my_loc;

var longpress;
var start;
var txtin;
var btnin;
var btncncl;

var loc_options = {
    // enableHighAccuracy: true,
    timeout: 5000,
    maximumAge: 0
};

var markers = [];

var current_edited_marker;
var marker_input;

var state = 'follow';

function createSelfMarker () {
    myloc = new google.maps.Marker({
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

function startMarkerEdition (pointed, init) {
    if (pointed.infoWindow && init == "") {
        pointed.infoWindow.open(pointed.get('map'), pointed);
    } else {
        current_edited_marker = pointed;
        marker_input.style.display = 'flex';
        txtin.value = init;
        txtin.focus();
    }
}

function createPointedMarker (latLng) {
    let pointed = new google.maps.Marker({
        icon: {
            path: google.maps.SymbolPath.CIRCLE,
            fillColor: 'red',
            fillOpacity: 1,
            scale: 8,
            strokeColor: 'black',
            strokeWeight: 2
        },
        position: latLng,
        map
    });

    pointed.addListener('click', function () {
        closeAllInfoWindows ();
        startMarkerEdition (pointed, "");
    });
    markers.push(pointed);

}


function refresh_current (position) {
    lat = position.coords.latitude;
    lng = position.coords.longitude;

    myloc.setPosition({lat, lng});

    if (state == 'follow') {
        map.setCenter({lat, lng});
    }

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

function closeAllInfoWindows () {
    markers.forEach(function (m) {
        if (m.infoWindow) m.infoWindow.close ();
    });
}

function initMap () {

    map = new google.maps.Map(document.getElementById('map'), {
        zoom: 15
    });
    locate_btn = document.getElementById('locate');
    infowindow = new google.maps.InfoWindow();
    myloc = createSelfMarker ();
    navigator.geolocation.watchPosition(refresh_current, refresh_error, loc_options);
    init ();
    enableOrientationArrow();
    locate.addEventListener('click', function () {
        state = 'follow';
        map.setCenter({lat, lng});
        map.setZoom(Math.max(map.getZoom(),17));
        navigator.geolocation.getCurrentPosition(
            refresh_current, refresh_error, loc_options);
    });

    marker_input = document.getElementById('marker-input');

    txtin = document.getElementById('marker-text-input');
    btnin = document.getElementById('marker-btn-input');
    btncncl = document.getElementById('marker-btn-cancel');

    var createInfoW = function () {
        if (txtin.value != "") {
            let v = txtin.value;
            let infodiv = document.createElement('div');
            infodiv.innerHTML = v;
            let c = current_edited_marker;

            infodiv.addEventListener('dblclick', function () {
                startMarkerEdition(c, v);
            });

            if (c.infoWindow) {
                c.infoWindow.setContent(infodiv);
            } else {
                let infowindow = new google.maps.InfoWindow({
                    content: infodiv
                });
                current_edited_marker.infoWindow = infowindow;
            }
            txtin.value = "";
            marker_input.style.display = 'none';
        }
    }

    txtin.addEventListener('keyup', function (e) {
        if (e.keyCode == 13) {
            createInfoW ();
        }
    });

    btnin.addEventListener('click', function () {
        createInfoW ();
    });

    btncncl.addEventListener('click', function () {
        txtin.value = "";
        marker_input.style.display = 'none';
    });

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

function drag () {
    state = 'static';
}



function init() {

    var searchBox = new google.maps.places.SearchBox(
        document.getElementById('pac-input'));

    map.controls[google.maps.ControlPosition.TOP_CENTER]
        .push(document.getElementById('pac-input'));
    map.controls[google.maps.ControlPosition.TOP_CENTER]
        .push(document.getElementById('locate'));
    map.addListener('drag', drag);


    google.maps.event.addListener(map, 'click', function (e) {
        closeAllInfoWindows ();
        if (longpress) {
            createPointedMarker(e.latLng);
        }
    });


    map.addListener('mousedown', function(event){
        start = new Date().getTime();
    });

    map.addListener('mouseup', function(event){
        end = new Date().getTime();
        longpress = (end - start < 500) ? false : true;
    });

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

                marker.addListener('click', function() {
                    infowindow.open(marker.get('map'), marker);
                });

            }(place));

        }
        state = 'static';
        map.fitBounds(bounds);
        searchBox.set('map', map);
        map.setZoom(Math.min(map.getZoom(),20));

    });
}
// google.maps.event.addDomListener(window, 'load', init);
