
Some information that may be of use -

Because NextCloud Note IDs are not unique, there is a danger of duplicating notes if you "re-join" an existing connection. This can be prevented by ensuring that the same note does not exist at both ends before initiating a Join. Possibly clearing out (selectively ?) notes at one end or the other.


Removing this note will break an existing connection and require a re-join, so please be careful to not remove or change this note unintentionally. If you do need to establish a new connection, you must manually remove this note from both ends.


NextCloud and Tomboy use different markup. Some markup does translate well, some does not. In all known problem cases, the markup but not the content will be lost in full sync cycle. Specifically -


* **Highlight** do not use in tomboy-ng if you will be changing the note in Nextcloud, the markup will be lost.
* **Large fonts** used in a normal paragraph, available in tomboy-ng but not NextCloud.
* **Small font** may be preserved but not rendered as small in NextCloud.


Please see the tomboy-ng github wiki page on NextCloud for further information.

