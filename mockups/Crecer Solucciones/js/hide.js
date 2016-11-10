 var $burguerButton = document.getElementById('burguer-button');
 var $menu = document.getElementById('menu');
 $estado =false;

      $burguerButton.addEventListener('click', toggleMenu);//nombre de la funcion 

      function toggleMenu(){
        $menu.classList.toggle('active')//nombre del a clase .css
        if (!$estado) {
        	$burguerButton.classList.remove('icon-indent-increase')
        	$burguerButton.classList.add('icon-indent-decrease')
        	$estado =true;

        }
        else{
        	$burguerButton.classList.remove('icon-indent-decrease')
        	$burguerButton.classList.add('icon-indent-increase')
        	$estado =false;
        }
        
      }
  