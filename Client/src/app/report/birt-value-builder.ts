

import { parseDateWithoutTimestamp } from '../common/utilities/date-utilities';

export function  getValuesForBirt(filter, values ) {
    const birtValues = {};
    Object.keys(values).forEach( K => {
        if (values[K].length || Object.keys(values[K]).length) {
            const TYPE = getTypeFromFilters(K, filter);
            birtValues[K] = getValueForFilterType( values[K], TYPE);
        }
    });
    return birtValues;
}

function getValueForFilterType(value, filterType) {
    if (filterType === 'EQUAL' || filterType === 'LIKE') {
        return getValueForLikeandEqual(value);
    } else if ( filterType === 'IN') {
        return addCriteriaForIN(value);
    }  else if ( filterType === 'BETWEEN') {
        return addCriteriaForBETWEEN(value);
    } else if ( filterType === 'BIRTEQUAL') {
        return addCriteriaForSINGLEDATE(value.from);
    }

}
function addCriteriaForIN(values: Array<any>) {
    let inCriteria = '(';
    values.forEach(value => inCriteria += addSingleQuotes(value) + ',');
    inCriteria = removeTrailingComma(inCriteria) + ')';
    return inCriteria;
}

function getTypeFromFilters(key: string, filters: Array<any>) {
   const filter = filters.find(F => F.columnName === key);
   return filter.queryType;
}

function getValueForLikeandEqual( value: Array<any>) {
    return value[0];
}

function addCriteriaForBETWEEN(values: any) {
    let TO = '';
    let FROM = '';
    let BETWEEN_CRITERIA = '';
    if (values.to) {
        TO = formatDate(values.to);
    }
    if (values.from) {
        FROM = formatDate(values.from);
    }
    if (TO !== '' && FROM !== '') {
        BETWEEN_CRITERIA = 'BETWEEN' + addSpace() + FROM + addSpace() + 'AND' + addSpace() + TO;
    } else if (TO === '' && FROM !== '') {
        BETWEEN_CRITERIA = '>' + addSpace() + FROM;
    } else if (TO !== '' && FROM === '') {
        BETWEEN_CRITERIA = '<' + addSpace() + TO;
    }
    return BETWEEN_CRITERIA;
}

function addSpace() {
    return ' ';
}

function removeTrailingComma(value) {
    return value.slice(0, value.length - 1);
}

function addSingleQuotes(value) {
    // tslint:disable: quotemark
    value = value.toString().replace(/'/g, "\\'");
    return "'" + value + "'";
}
export function formatDate(date) {
    date = parseDateWithoutTimestamp(date);
    return addSingleQuotes(date);
}

function addCriteriaForSINGLEDATE(value) {
    value = new Date(value);
    return ('0' + (value.getMonth() + 1)).slice(-2) + '/' +  ('0' + value.getDate()).slice(-2) + '/' + value.getFullYear();
}
