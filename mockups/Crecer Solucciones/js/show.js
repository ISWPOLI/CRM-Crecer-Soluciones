     
$('#Showbutton').click(function(event){
	
	var p = $('#Showbutton').text();

	
	if (p =="mostar mas") {
		$('#Showbutton').text("ocultar");
	} else{
		$('#Showbutton').text("mostar mas");
		$('#Showbutton').attr({'href':"#Showbutton"})
	};
	
	$('.descripcion').toggle("display")
	

});

$( '#tributaria' ).click(function() {

  $( 'section' ).hide();
  $( "#Atributaria" ).toggle( "explode" );

});

$( '#contable' ).click(function() {
	$( 'section' ).hide();
  $( "#Acontable" ).toggle( "explode" );
});


