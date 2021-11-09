# blogger2hugo

Pequeño Script en F# que monté para _mover_ todos los posts de mi antiguo blog en Blogger, desde una copia de seguridad en XML que te ofrece la plataforma, a un conjunto de archivos MD pensados en las posibilidades de [HUGO](https://gohugo.io/).

El script es muy mejorable, pero buscaba poder mover lo máximo posible sin dedicar mucho tiempo. Logro conseguido. Lo puedes usar como base para tu migración, llegado el caso.

Para poder mantener las referencias entre entradas de un sitio a otro, hago la lectura del XML y mantengo un diccionario que facilite fabricar esas referencias.

Por mi parte ya he obtenido lo que necesitaba. Procederé a realizar alguna limpieza posterior y la descarga de imágenes para incluirlas en el propio sitio, que aún mantiene las referencias originales a blogger.
