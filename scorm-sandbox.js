var api = window.API_1484_11 = {};

api.Initialize = function() {
    return true;
}

api.Terminate = function() {
    return true;
}

api.GetValue = function(k) {
    return values[k] || '';
}

var values = {};

values['cmi.student_id'] = 'StudentId';
values['cmi.learner_id'] = 'LearnerId';

api.SetValue = function(k, v) {
    values[k] = v;
    return true;
}

api.Commit = function() {
    return true;
}

api.GetLastError = function() {
    return 0;
}

api.GetErrorString = function(e) {
    return 'GetErrorString: ' + e;
}

api.GetDiagnostic = function() {
    return 'GetDiagnostic: ...';
}
