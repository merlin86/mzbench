import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchActions from '../actions/MZBenchActions';

const CHANGE_EVENT = 'metrics_change';

let data = {
    benchId: undefined,
    guid: undefined,
    is_loaded: false,
    starting_date: undefined,
    map: new Map([]),
    metrics: undefined
};

function _clearData() {
    data.is_loaded = false;
    data.starting_date = undefined;
    data.map.clear();
}

function _updateData(rawData) {
    const updates = rawData.split("\n");
    updates.forEach((update) => {
        _applyUpdate(update);
    });
}

function _applyUpdate(update) {
    const tokens = update.split("\t");
    const date = Number.parseInt(tokens[0]);
    const values = data.metrics.map((metric, i) => {
        const val = Number.parseFloat(tokens[i+1]);
        if(!Number.isNaN(date) && !Number.isNaN(val)) {
            _addObservation(metric, { date: date, value: val});
        }
    });
}

function _addObservation(metric, observation) {
    if(data.map.has(metric)) {
        _updateMetric(metric, observation);
    } else {
        _createMetric(metric, observation);
    }
}

function _createMetric(metric, observation) {
    if(!data.starting_date) {
        data.starting_date = observation.date;
    }
    
    data.map.set(metric, new Array({"date": _convertDate(observation.date), "value": observation.value}));
}

function _updateMetric(metric, observation) {
    data.map.get(metric).push({"date": _convertDate(observation.date), "value": observation.value});
}

function _convertDate(rawDate) {
    return rawDate - data.starting_date;
}

class MetricsStore extends EventEmitter {
    constructor() {
        super();
        this.setMaxListeners(Infinity);
    }
    
    emitChange() {
        return this.emit(CHANGE_EVENT);
    }

    onChange(callback) {
        this.on(CHANGE_EVENT, callback);
    }

    off(callback) {
        this.removeListener(CHANGE_EVENT, callback);
    }
    
    getCurrentBenchId() {
        return data.benchId;
    }
    
    changeCurrentBench(benchId, GUID) {
        data.benchId = benchId;
        data.guid = GUID;
        _clearData();
    }
    
    isDataLoaded() {
        return data.is_loaded;
    }
    
    updateMetricData(benchId, guid, rawData) {
        if(data.benchId == benchId && data.guid == guid) {
            _updateData(rawData);
        }
    }
    
    metricsBatchFinished(benchId, guid) {
        if(data.benchId == benchId && data.guid == guid) {
            data.is_loaded = true;
        }
    }
    
    getMetricData(metric) {
        if(data.map.has(metric)) {
            return data.map.get(metric);
        } else {
            return [];
        }
    }

    getMetricMaxDate(metric) {
        if(data.map.has(metric)) {
            let m = data.map.get(metric);
            return m[m.length - 1]["date"];
        } else {
            return 0;
        }
    }

    setMetrics(benchId, guid, rawData) {
        if(data.benchId == benchId && data.guid == guid) {
            data.metrics = rawData.split("\t");
        }
    }
};

var _MetricsStore = new MetricsStore();
export default _MetricsStore;

_MetricsStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
        case ActionTypes.METRIC_NAMES:
            _MetricsStore.setMetrics(action.bench, action.guid, action.data);
            break;

        case ActionTypes.METRICS_UPDATE:
            _MetricsStore.updateMetricData(action.bench, action.guid, action.data);
            _MetricsStore.emitChange();
            break;

        case ActionTypes.METRICS_BATCH_FINISHED:
            _MetricsStore.metricsBatchFinished(action.bench, action.guid);
            _MetricsStore.emitChange();
            break;

        default:
    }
});