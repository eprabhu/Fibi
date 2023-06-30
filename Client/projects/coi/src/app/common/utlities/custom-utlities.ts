import { DEFAULT_UNIT_FORMAT } from "../../app-constants";

/**
 * Takes a string and replaces words with values if they are present in the given object. null & empty brackets will be removed by default.
 * @param formatString
 * @param object
 */
export function replaceFormatStringWithValue(formatString: string, object: any): string {
    if (!formatString || !object) { return ''; }
    Object.keys(object).forEach(key => {
        formatString = formatString.replace(key, object[key] || '');
    });
    return formatString.replace(/null/g, '').replace('()', '');
}

/**
 * returns "sponsorCode - sponsorName (acronym)" -> Endpoint format based default value
 * @param sponsor
 */
export function getSponsorSearchDefaultValue(sponsor: any): string {
    return replaceFormatStringWithValue(DEFAULT_UNIT_FORMAT, sponsor);
}
