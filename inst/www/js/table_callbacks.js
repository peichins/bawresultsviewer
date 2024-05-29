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

    var listen_link = `<a href='https://${fixed_params['web_host']}/listen/${arid}?start=${Math.max(0, offset_seconds - 1)}&end=${Math.min(fixed_params['recording_duration'], offset_seconds + 1 + fixed_params['clip_duration'])}' target='_blank'>Listen</a>`
    var download_link = `<a href='https://${fixed_params['api_host']}/audio_recordings/${arid}/media.wav?start_offset=${offset_seconds}&end_offset=${offset_seconds + fixed_params['clip_duration']}${user_token_str}' target='_blank'>Download</a>`;
    // Get user_token from the input element

    $(`td:eq(${rownums['listen']})`, row).html(listen_link);
    $(`td:eq(${rownums['download']})`, row).html(download_link);



}


$(document).ready(function() {
  console.log("one")
  $("#user_token").on("input", function() {
    console.log("two")
    $('#detailsTable table').DataTable().draw()
  });
});

