$(function() {

    var msg = $('#message'),
        img = $('#image>img');

    function showItems(resp) {
        var imgW = img.width(),
            imgH = img.height();

        msg.text(JSON.stringify(resp));
        msg.removeClass().addClass('alert alert-success').show();
        resp.sort(function(a, b) {
          return (b.box.h * b.box.w) - (a.box.h * a.box.w);
        });
        resp.forEach(function(item) {
            var
                left = (item.box.x - item.box.w / 2) * imgW,
                top = (item.box.y - item.box.h / 2) * imgH,
                width = item.box.w * imgW,
                height = item.box.h * imgH,
                labelText = (Math.round(100 * item.confidence)) + '% ' + item.name + ' (' + item.class + ')',
                label = $('<div>').addClass('itemLabel').text(labelText),
                box = $('<div>').addClass('item')
                .attr('title', labelText)
                .css('left', left + 'px')
                .css('top', top + 'px')
                .css('width', width + 'px')
                .css('height', height + 'px');

            box.append(label);
            $('#image').append(box);
        });
    };

    function showFile(input) {
        if (input.files && input.files[0]) {
            var reader = new FileReader(),
                data = new FormData();

            $('.item').remove();
            reader.onload = function(e) {
                img.attr('src', e.target.result).show();;
            };
            reader.readAsDataURL(input.files[0]);

            data.append('image', input.files[0]);

            $.ajax({
                type: "POST",
                url: "/findItems",
                data: data,
                cache: false,
                contentType: false,
                processData: false,
                success: showItems
            });
        };
    };


    $('#imageFile').change(function() {
        img.hide();
        msg.hide();
        showFile(this);
    });

});
