{application, ucengine,
[{description, "ucengine"},
 {vsn, "0.1"},
 {modules, [
    ucengine,
    ucengine_app,
    ucengine_event,
    ucengine_sup
  ]},
 {registered,[ucengine]},
 {applications, [kernel,stdlib]},
 {mod, {ucengine_app,[]}}
]}.

