# Mini-projet Erlang

## Compilation & Utilisation

Pour compiler les quatres modules de ce projet il faut exécuter les commandes
suivantes depuis le répertoire courrant (sauf la première, qui ne sert qu'à montrer 
la version de erlang avec laquelle j'ai compilé mon code):

```
$ cd src
$ erl
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> erlang:system_info(otp_release).
"22"
2> c(matrix).
{ok,matrix}
3> c(check_stamp).
{ok,check_stamp}
4> c(messenger).
{ok,messenger}
5> c(test).
{ok,test}
```

Ici le code a été compilé sur Ubuntu 20.

Pour utiliser le programme, deux options sont possibles:

Pour lancer N processus avec M communications aléatoires entre eux il faut
executer la fonction ```main``` de ```test.erl``` avec les arguments suivants:

```
6> test:main(4, 100).
```

Où 4 est le nombre de porcessus N et 100 le nombre M de communications.

Pour lancer le scénario déjà écrit, qui corréspond au scénario dans l'image
```exemple.png``` du répertoire (il s'agit de l'exemple de la fin du chapitre
2 du cours) il suffit de faire

```
7> test:main().
```

Enfin, il est également possible de paramétrer les bornes suppérieurs des
délais aléatoires appliquées sur la communication entre processus (afin d'avoir
des messages qui doivent être reportés par notre mécanisme d'horloge
matricielle)

```
8> test:main (4, 100, 250, 500)
```

Pour le détail de ces deux derniers paramètres, il faut lire la déscription des
fonctions ```test:main/4``` et ```messenger:send/5```.

## Comprendre le résultat affiché

Il y a 5 types d'affichages possibles; nous les expliquons ici. Chaque type
d'affichage commence et termine par une chaine ```#```.

### Evenement local

```
========================================================
2 has a local event; stamped:
[[5,1,3,1],
 [0,5,1,0],
 [1,2,12,1],
 [0,0,2,3]]
========================================================
```

Notons que pour les événements locaux, l'estampile affichée corréspond déjà
à l'enstampile mise à jour.

### Message envoyé

```
========================================================
0 sends a message to 3; Local Stamp of 0: 
[[14,2,3,3],
 [1,8,1,0],
 [1,2,11,1],
 [2,1,2,12]]

 sent updated stamp: 
[[15,2,3,4],
 [1,8,1,0],
 [1,2,11,1],
 [2,1,2,12]]
========================================================
```

Pour les messages envoyés, on a d'abord l'état de l'horloge du processus sans
changements, puis celle avec les changements corréspondant à l'envoi du
message; qui est donc envoyé au processus qui doit le recevoir.

### Message reçu

```
========================================================
1 received a message from 2; Local Stamp of 1: 
[[7,2,3,1],
 [1,9,2,0],
 [0,2,7,1],
 [1,1,2,8]]

 Received Stamp of 2:
[[5,1,3,1],
 [0,5,1,0],
 [1,3,13,1],
 [0,0,2,3]]

 Updated Stamp of 1: 
[[7,2,3,1],
 [1,10,2,0],
 [1,3,13,1],
 [1,1,2,8]]
========================================================
```

Pour la réception d'un message, on a d'abord l'état initial de l'horloge du
processus qui reçoit le message, puis l'estampile qu'il reçoit et enfin l'état
de son horloge après les mise à jour.

Pour le cas d'un déséquencement, le mot ```DESYNCHRONIZED``` est affiché ainsi
que la raison pour laquelle ce déséquencement est advenu.

```
========================================================
2 received a message from 0 [(EM[1,2] > HM[1,2]) => DESYNCHRONIZED]
 Local Stamp of 2: 
[[6,2,2],
 [1,5,2],
 [1,2,7]]

 Received Stamp of 0:
[[8,2,3],
 [2,9,3],
 [1,2,5]]
========================================================
```

### Message envoyé avec un délai

```
========================================================
(SENT WITH DELAY) 2 received a message from 0; Local Stamp of 2: 
[[6,2,2],
 [1,8,3],
 [1,2,8]]

 Received Stamp of 0:
[[8,2,3],
 [2,9,3],
 [1,2,5]]

 Updated Stamp of 2: 
[[8,2,3],
 [2,9,3],
 [1,2,9]]
========================================================
```

Si un message est envoyé après avoir été stocké dans un buffer (pour éviter un
déséquencement) le mot clé ```SENT WITH DELAY``` est affiché.

### Message testé

```
========================================================
0 tested bufferized stamp from 2: Local Stamp of 0 
[[39,8,8],
 [7,37,7],
 [9,8,37]]

 Received Stamp of 2 
[[33,7,7],
 [5,28,7],
 [11,8,40]]

 (still non-causal.) 
========================================================
```

Enfin, ce message corréspond au cas où un message a été stocké pour éviter un
déséquencement. Ici, il a été stocké par ```0``` et envoyé par ```2```. La
phrase ```still non-causal``` signifie que ce message a été testé par ```0```
une nouvelle fois mais que le message ne peut pas encore être reçu puisqu'il
engendrerait encore un déséquencement.

## Courte explication du fonctionnement du programme

Ce programme permet de créer ```N``` processus avec ```M``` communications
entre eux.

Chaque processus crée rentre dans une fonction qui attend l'arrivé d'un
message graçe à ```receive```. Deux messages peuvent alors arriver: un message
signalisé par l'atome ```send```, qui corréspond à une demande d'envoie d'un
message depuis ce processus vers un autre; et un message signalisé par l'atome
```msg``` qui corréspond à la réception d'un message.

Nous utilisons l'atome ```send``` pour nous permettre de demander à un
processus d'envoyer un message de sorte que ce soit bien lui qui l'envoie
(depuis son pid) et non pas le pid du processus courrant ou on donne ces
directives.

Pour implémenter les horloges matricielles, un fichier ```matrix.erl```
implemente une structure de matrice avec des ```array``` d'erlang. Le fichier
```check_stamp.erl``` contient toutes les fonctions nécessaire à la
vérification qu'un message envoyé n'est pas désynchronisé. Enfin le fichier
```messenger.erl``` contient toutes les fonctions qui gèrent la génération de
processus, ainsi que les fonctions qui permettent à un processus de mettre
à jour son estampile; envoyer des messages avec un délai; afficher toutes les
informations nécessaires dans le terminal ect...

Pour les messages qui ne respectent pas les conditions qu'il faut respecter
à la réception d'un message, nous les gardons dans une file (```queue``` en
erlang) interne au processus qui l'a reçu et nous vérifions, à chaque nouveau
message, si les estampiles dans cette pile peuvent être envoyés ou si elles
doivent encore être retardées. Si elles peuvent être envoyés, nous les
envoyons; sinon on les garde dans la pile.
