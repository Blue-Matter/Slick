## In a file named 'js4checkbox.js' in your app folder :
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