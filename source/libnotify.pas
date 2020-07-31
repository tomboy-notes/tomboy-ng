{ libnotify binding for Free Pascal

  Copyright (C) 2011 Ido Kanner idokan at@at gmail dot.dot com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit libnotify;

{$mode fpc}{$PACKRECORDS C}

interface
uses ctypes, glib2, gdk2pixbuf;

const
  NOTIFY_LIBRARY = 'libnotify';

// Should be part of GTK but it is not binded to FPC :(
type
 GVariant  = record end;
 GfreeFunc = procedure(data : gpointer); cdecl;

{ notification.h }
const
 (**
 * NOTIFY_EXPIRES_DEFAULT:
 *
 * The default expiration time on a notification.
 *)
 NOTIFY_EXPIRES_DEFAULT = -1;

 (**
 * NOTIFY_EXPIRES_NEVER:
 *
 * The notification never expires. It stays open until closed by the calling API
 * or the user.
 *)
 NOTIFY_EXPIRES_NEVER   = 0;

type
  P_NotifyNotificationPrivate = ^T_NotifyNotificationPrivate;
  T_NotifyNotificationPrivate = record end;

  PNotifyNotificationPrivate = P_NotifyNotificationPrivate;
  NotifyNotificationPrivate  = T_NotifyNotificationPrivate;

  P_NotifyNotification = ^T_NotifyNotification;
  T_NotifyNotification = record
    parent_object : TGObject;
    priv          : PNotifyNotificationPrivate;
  end;

  PNotifyNotification = P_NotifyNotification;
  TNotifyNotification = T_NotifyNotification;

  TNotificationProc = procedure (Notification : PNotifyNotification); cdecl;

  P_NotifyNotificationClass = ^T_NotifyNotificationClass;
  T_NotifyNotificationClass = record
    parent_class : TGObjectClass;
    // Signals
    Notification : TNotificationProc;
  end;

  PNotifyNotificationClass = P_NotifyNotificationClass;
  TNotifyNotificationClass = T_NotifyNotificationClass;

function  notify_notification_get_type : GType; cdecl; external NOTIFY_LIBRARY;

function m_notify_type_notification : GType; cdecl; inline;
function M_NOTIFY_NOTIFICATION(o : pointer) : PGTypeInstance; cdecl; inline;
function M_NOTIFY_NOTIFICATION_CLASS(k : Pointer) : Pointer; cdecl; inline;
function M_NOTIFY_IS_NOTIFICATION(o : Pointer) : Boolean; cdecl; inline;
function M_NOTIFY_IS_NOTIFICATION_CLASS(k : pointer) : Boolean; cdecl; inline;
function M_NOTIFY_NOTIFICATION_GET_CLASS(o : Pointer) : PGTypeClass; cdecl; inline;

const
 (**
 * NotifyUrgency:
 * @NOTIFY_URGENCY_LOW: Low urgency. Used for unimportant notifications.
 * @NOTIFY_URGENCY_NORMAL: Normal urgency. Used for most standard notifications.
 * @NOTIFY_URGENCY_CRITICAL: Critical urgency. Used for very important notifications.
 *
 * The urgency level of the notification.
 *)
 NOTIFY_URGENCY_LOW      = 0;
 NOTIFY_URGENCY_NORMAL   = 1;
 NOTIFY_URGENCY_CRITICAL = 2;

type
  NotifyUrgency = cint;

 (**
 * NotifyActionCallback:
 * @notification:
 * @action:
 * @user_data:
 *
 * An action callback function.
 *)
 NotifyActionCallback = procedure(notification : PNotifyNotification;
                                  action       : PChar;
                                  user_data    : gpointer); cdecl;
(*
/**
 * NOTIFY_ACTION_CALLBACK:
 * @func: The function to cast.
 *
 * A convenience macro for casting a function to a #NotifyActionCallback. This
 * is much like G_CALLBACK().
 */
#define NOTIFY_ACTION_CALLBACK(func) ((NotifyActionCallback)(func))
*)

function notify_notification_new(summary, body, icon : PChar) : PNotifyNotification;
 cdecl; external NOTIFY_LIBRARY;

function notify_notification_update(notification : PNotifyNotification;
                                    summary, body, icon : PChar) : gboolean;
 cdecl; external NOTIFY_LIBRARY;

function notify_notification_show(notification : PNotifyNotification;
                                  error        : PPGError)             : gboolean;
  cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_timeout(notification : PNotifyNotification;
                                          timeout      : gint);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_category(notification : PNotifyNotification;
                                           category     : PChar);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_urgency(notification : PNotifyNotification;
                                          urgency      : NotifyUrgency);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_icon_from_pixbuf(
                                        notification : PNotifyNotification;
                                        icon         : PGdkPixbuf);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_image_from_pixbuf(
                                        notification : PNotifyNotification;
                                        pixbuf       : PGdkPixbuf);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_hint_int32(notification : PNotifyNotification;
                                             key          : PChar;
                                             value        : gint);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_hint_uint32(notification : PNotifyNotification;
                                              key          : PChar;
                                              value        : guint);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_hint_double(notification : PNotifyNotification;
                                              key          : PChar;
                                              value        : gdouble);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_hint_string(notification : PNotifyNotification;
                                              key          : PChar;
                                              value        : PChar);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_hint_byte(notification : PNotifyNotification;
                                            key          : PChar;
                                            value        : guchar);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_hint_byte_array(
                                             notification : PNotifyNotification;
                                             key          : PChar;
                                             value        : Pguchar;
                                             len          : gsize);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_hint(notification : PNotifyNotification;
                                       key          : PChar;
                                       value        : GVariant);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_set_app_name(notification : PNotifyNotification;
                                           app_name     : PChar);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_clear_hints(notification : PNotifyNotification);
  cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_add_action(notification : PNotifyNotification;
                                         action,
                                         label_       : PChar;
                                         callback     : NotifyActionCallback;
                                         user_data    : gpointer;
                                         free_funch   : GFreeFunc);
 cdecl; external NOTIFY_LIBRARY;

procedure notify_notification_clear_actions(notification : PNotifyNotification);
  cdecl; external NOTIFY_LIBRARY;

function notify_notification_close(notification : PNotifyNotification;
                                   error        : PPGError)            : gboolean;
 cdecl; external NOTIFY_LIBRARY;

function notify_notification_get_closed_reason(
                                     notification : PNotifyNotification) : gint;
 cdecl; external NOTIFY_LIBRARY;

{ notify-enum-types.h }
(* enumerations from "notification.h" *)
function notify_urgency_get_type : GType;
 cdecl; external NOTIFY_LIBRARY;

function M_NOTIFY_TYPE_URGENCY : GType; cdecl; inline;

{ notify-features.h }
(* compile time version *)
const
  NOTIFY_VERSION_MAJOR = 0;
  NOTIFY_VERSION_MINOR = 7;
  NOTIFY_VERSION_MICRO = 3;

(* check whether a version equal to or greater than
 * major.minor.micro is present.
 *)
function M_NOTIFY_CHECK_VERSION(major, minor, micro : cint) : Boolean; cdecl; inline;

{ notify.h }

function notify_init(app_name : PChar) : gboolean;
 cdecl; external NOTIFY_LIBRARY;

procedure notify_uninit; cdecl; external NOTIFY_LIBRARY;
function notify_is_initted : gboolean; cdecl; external NOTIFY_LIBRARY;
function notify_get_app_name : PChar; cdecl; external NOTIFY_LIBRARY;
procedure notify_set_app_name(app_name : PChar); cdecl; external NOTIFY_LIBRARY;
function notify_get_server_caps : PGList; cdecl; external NOTIFY_LIBRARY;
function notify_get_server_info(ret_name, ret_vendor, ret_version : PPChar) : gboolean;
 cdecl; external NOTIFY_LIBRARY;

implementation

function m_notify_type_notification : GType; cdecl;
begin
  m_notify_type_notification := notify_notification_get_type;
end;

function M_NOTIFY_NOTIFICATION(o : pointer): PGTypeInstance; cdecl;
begin
//  #define NOTIFY_NOTIFICATION(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), NOTIFY_TYPE_NOTIFICATION, NotifyNotification))
  M_NOTIFY_NOTIFICATION := G_TYPE_CHECK_INSTANCE_CAST(o, m_notify_type_notification);
end;

function M_NOTIFY_NOTIFICATION_CLASS(k: Pointer): Pointer; cdecl;
begin
 //#define NOTIFY_NOTIFICATION_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), NOTIFY_TYPE_NOTIFICATION, NotifyNotificationClass))
 M_NOTIFY_NOTIFICATION_CLASS := G_TYPE_CHECK_CLASS_CAST(k, m_notify_type_notification);
end;

function M_NOTIFY_IS_NOTIFICATION(o: Pointer): Boolean; cdecl;
begin
  // #define NOTIFY_IS_NOTIFICATION(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), NOTIFY_TYPE_NOTIFICATION))
  M_NOTIFY_IS_NOTIFICATION := G_TYPE_CHECK_INSTANCE_TYPE(o, m_notify_type_notification);
end;

function M_NOTIFY_IS_NOTIFICATION_CLASS(k: pointer): Boolean; cdecl;
begin
  // #define NOTIFY_IS_NOTIFICATION_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), NOTIFY_TYPE_NOTIFICATION))
  M_NOTIFY_IS_NOTIFICATION_CLASS := G_TYPE_CHECK_CLASS_TYPE(k, m_notify_type_notification);
end;

function M_NOTIFY_NOTIFICATION_GET_CLASS(o: Pointer): PGTypeClass; cdecl;
begin
  // #define NOTIFY_NOTIFICATION_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), NOTIFY_TYPE_NOTIFICATION, NotifyNotificationClass))
  M_NOTIFY_NOTIFICATION_GET_CLASS := G_TYPE_INSTANCE_GET_CLASS(o, m_notify_type_notification);
end;

function M_NOTIFY_TYPE_URGENCY: GType; cdecl;
begin
  M_NOTIFY_TYPE_URGENCY := notify_urgency_get_type;
end;

function M_NOTIFY_CHECK_VERSION(major, minor, micro: cint): Boolean; cdecl;
begin
  {
  #define NOTIFY_CHECK_VERSION(major,minor,micro) \
    (NOTIFY_VERSION_MAJOR > (major) || \
     (NOTIFY_VERSION_MAJOR == (major) && NOTIFY_VERSION_MINOR > (minor)) || \
     (NOTIFY_VERSION_MAJOR == (major) && NOTIFY_VERSION_MINOR == (minor) && \
      NOTIFY_VERSION_MICRO >= (micro)))
  }
  M_NOTIFY_CHECK_VERSION := ((NOTIFY_VERSION_MAJOR  >  major)  or
                             ((NOTIFY_VERSION_MAJOR  = major)  and
                              (NOTIFY_VERSION_MINOR >  minor)) or
                             ((NOTIFY_VERSION_MAJOR  = major)  and
                              (NOTIFY_VERSION_MINOR  = minor)  and
                              (NOTIFY_VERSION_MICRO >= micro)));
end;

end.

