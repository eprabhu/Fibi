import { DEFAULT_UNIT_FORMAT } from '../../app-constants';

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

/**
 * Formats the amount to a string representation with the appropriate number of decimal places.
 * If the amount is an integer, it will be displayed with two decimal places
 *          --(e.g., 75 -> '75.00').
 * If the amount has decimal places, it will be displayed with the original number of decimal places
 *          --(e.g., 75.767 -> '75.767').
 *
 * @param amount The amount to be formatted.
 * @returns The formatted amount as a string.
 */
export function getFormattedAmount(amount: number): string {
    return amount.toFixed(amount % 1 === 0 ? 2 : amount.toString().split('.')[1]?.length || 0);
  }

