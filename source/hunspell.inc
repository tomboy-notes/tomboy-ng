
{ This include file provides the basic Object Pascal bindings to the Hunspell
  spelling library. It was built with reference to the hunspell.h, please see
  https://github.com/hunspell/hunspell/blob/master/src/hunspell/hunspell.h
  and so inherits the Hunspell license.

  The author of this particular file is David Bannon but the file contains
  no original code, just the names and parameter lists of the public hunspell
  functions so no copyright is claimed. Nor is any warranty of any kind
  expressed or implied.

}

(*
 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Hunspell, based on MySpell.
 *
 * The Initial Developers of the Original Code are
 * Kevin Hendricks (MySpell) and Németh László (Hunspell).
 * Portions created by the Initial Developers are Copyright (C) 2002-2005
 * the Initial Developers. All Rights Reserved.
 *
 * Contributor(s): David Einstein, Davide Prina, Giuseppe Modugno,
 * Gianluca Turconi, Simon Brouwer, Noll János, Bíró Árpád,
 * Goldman Eleonóra, Sarlós Tamás, Bencsáth Boldizsár, Halácsy Péter,
 * Dvornik László, Gefferth András, Nagy Viktor, Varga Dániel, Chris Halls,
 * Rene Engelhard, Bram Moolenaar, Dafydd Jones, Harri Pitkänen
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK *****  *)



{ These types will be used, elsewhere, to declare pointer variables that will
  be set to point to the various functions in the library.  They are, therefore
  just pointers to a function. String parameters must be PChar because thats
  how Pascal passes strings to C code and they are marked cdecl. In the C code
  they apparently return a pointer to a Hunhandle structure, thats not our business,
  all we need to know is the pointer returned by create points to something.
}

type
  THunspell_create = function(affpath: PChar; dpath: PChar): Pointer; cdecl;

  THunspell_create_key = function(affpath, dpath, key : PChar) : Pointer; cdecl;        // UNTESTED

  THunspell_destroy = procedure(spell: Pointer); cdecl;

                    { load extra dictionaries (only dic files), returns 0 if
                    additional dictionary slots available, 1 if slots are now full }
  THunspell_add_dic = function(pHunspell : Pointer; dpath : PChar) : integer; cdecl;    // UNTESTED


                    { spellcheck word, ret 0 is bad spelling, non zero good to go }
  THunspell_spell = function(pHunspell: Pointer; word: PChar): Boolean; cdecl;

  THunspell_get_dic_encoding = function(pHunspell: Pointer): PChar; cdecl;              // UNTESTED

                    { pass a misspelt word and get back an array of suggestions, you
                    must free (unless no suggestions offered) the list when done with it. }
  THunspell_suggest = function(pHunspell: Pointer; out slst: PPChar; word: PChar): Integer; cdecl;

                    { morphological functions, I will leave that to someone who understands }
  THunspell_analyze = function(pHunspell: Pointer; var slst: PPChar; word: PChar): Integer; cdecl; // UNTESTED

                    { stemmer function ?? }
  THunspell_stem = function(pHunspell: Pointer; var slst: PPChar; word: PChar): Integer; cdecl;    // UNTESTED

  // int Hunspell_stem2(Hunhandle* pHunspell, char*** slst, char** desc, int n);
  // int Hunspell_generate2(Hunhandle* pHunspell, char*** slst, const char* word, char** desc, int n);

                    { Add to run time dictionary }
  THunspell_add = function(pHunspell: Pointer; word: PChar): Integer; cdecl;

                    { add word to the run-time dictionary with affix flags of
                    the example (a dictionary word): Hunspell will recognize
                    affixed forms of the new word, too. }
  // int Hunspell_add_with_affix(Hunhandle* pHunspell, const char* word, const char* example);

                    { remove word from the run-time dictionary  }
  THunspell_remove = function(spell: Pointer; word: PChar): Integer; cdecl;

                    { free suggestion lists }
  THunspell_free_list = procedure(spell: Pointer; var slst: PPChar; n: integer); cdecl;





