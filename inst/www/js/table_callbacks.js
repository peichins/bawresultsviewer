function updateRow(row, data, index, fixed_params, rownums) {

    console.log("four")
    var arid = data[rownums['arid']];
    var offset_seconds = data[rownums['offset_seconds']];

    var user_token = $("#user_token").val();
    if (user_token) {
        var user_token_str = `&user_token=${user_token}`;
    } else {
        var user_token_str = '';
    }

    listen_url = `https://${fixed_params['web_host']}/listen/${arid}?start=${offset_seconds}&end=${offset_seconds + fixed_params['clip_duration']}`
    download_url = `https://${fixed_params['api_host']}/audio_recordings/${arid}/media.wav?start_offset=${offset_seconds}&end_offset=${offset_seconds + fixed_params['clip_duration']}${user_token_str}`

    //var listen_link = `<a href='https://${fixed_params['web_host']}/listen/${arid}?start=${offset_seconds}&end=${offset_seconds + fixed_params['clip_duration']}' target='_blank' >Listen</a>`
    //var download_link = `<a href='https://${fixed_params['api_host']}/audio_recordings/${arid}/media.wav?start_offset=${offset_seconds}&end_offset=${offset_seconds + fixed_params['clip_duration']}${user_token_str}' target='_blank'>Download</a>`;
    // Get user_token from the input element

    //$(`td:eq(${rownums['listen']})`, row).html(listen_link);
    $(`td:eq(${rownums['listen']})`, row).append(createLink(listen_url, "Listen", row));

    //$(`td:eq(${rownums['download']})`, row).html(download_link);
    $(`td:eq(${rownums['download']})`, row).append(createLink(download_url, "Download", row));





}


$(document).ready(function() {
  console.log("one")
  $("#user_token").on("input", function() {
    console.log("two")
    $('#detailsTable table').DataTable().draw()
  });
});

function createLink(url, text) {
    // Create the link element
    var link = document.createElement('a');
    link.href = url;
    link.target = "_blank"; // Open in a new tab/window
    link.textContent = text;

    // Add an event listener to explicitly open in browser
    link.addEventListener('click', function(event) {
        event.preventDefault(); // Prevent default RStudio behavior
        window.open(url, '_blank'); // Force open in browser
    });

    return link;
}
