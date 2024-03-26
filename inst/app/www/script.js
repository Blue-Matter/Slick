$(document).ready(function(){
  $('input[name=Fil_PM_Det]').on('click', function(event){
    if($('input[name=Fil_PM_Det]:checked').length > 3){
      $(this).prop('checked', false);
    }
  });
  $('input[name=Fil_PM_Det]').on('click', function(event){
    if($('input[name=Fil_PM_Det]:checked').length == 0){
      $(this).prop('checked', true);
    }
  });
});

const customHref = function(link){

        // find all links
        const links = document.getElementsByTagName("a");

        // since it returns an object, iterate over each entries
        Object.entries(links).forEach( (elem, i) => {

                // match value attribute with input var
                if(elem[1].getAttribute("data-value") === link){

                        // if match, click link
                        elem[1].click()
                }

        });
}

var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });


        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
