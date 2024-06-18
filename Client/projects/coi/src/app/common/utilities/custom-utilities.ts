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
 * returns a string format of "<unitNumber> - <unitName>
 * @param sponsor
 */
export function getPersonLeadUnitDetails(unitData: any): string {
    if (unitData && (unitData.hasOwnProperty('homeUnit') || unitData.hasOwnProperty('homeUnitNumber')) && unitData.hasOwnProperty('homeUnitName')) {
        unitData['unitNumber'] = unitData.homeUnit || unitData.homeUnitNumber;
        unitData['unitName'] = unitData.homeUnitName;
    }
    return replaceFormatStringWithValue(DEFAULT_UNIT_FORMAT, unitData);
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
    if (amount) {
        return amount.toFixed(amount % 1 === 0 ? 2 : amount.toString().split('.')[1]?.length || 0);
    }
    return '0';
}
/**
 * '?' has to be added to path in function call; provided query param values are []
 * @param path : <String>current path where to redirect to. eg: path = 'agreement/agreementhome?agreementId='
 * @param queryParamKeys : Its accepts Array of all Keys queryParam eg: In awardId=8085, 'awardId' will be queryParamKey;
 * for paths without query params, pass queryParamKeys as [],
 * @param queryParamValues ; Its accepts Array of all Value, eg: In awardId=8085, '8085' is the queryParamValue;
 * for paths without query params, pass queryParamValues as [],
 * If index is 0, append key, value pair only, otherwise prefix '&' for each key value pair in the url
 */
export function openInNewTab(path: string, queryParamKeys: Array<any>, queryParamValues: Array<any>) {
    if (path && queryParamKeys && queryParamValues && queryParamKeys.length === queryParamValues.length) {
        let url = '';
        queryParamKeys.forEach((key, index) => {
            if (queryParamValues[index]) {
                (index === 0) ? url = key + '=' + queryParamValues[index] : url = url + '&' + key + '=' + queryParamValues[index];
            }
        });
        url = window.location.origin + window.location.pathname + '#/coi/' + path + url;
        window.location.hash.split(/[/?]/).includes('dashboard') ? window.open(url, '_self') : window.open(url, url);
    }
}

export function openSlider(sliderName: string = 'coi-slider'): void {
    document.getElementById(`${sliderName}-overlay`).style.display = 'block';
    document.getElementById(`${sliderName}-overlay`).classList.add('overlay');
    document.getElementById('COI_SCROLL').classList.add('overflow-hidden');

    setTimeout(() => {
        document.getElementById(sliderName).classList.add('slider-opened');
    });
}

export function closeSlider(sliderName: string = 'coi-slider'): void {
    document.getElementById(sliderName).classList.remove('slider-opened');

    setTimeout(() => {
        document.getElementById(`${sliderName}-overlay`).style.display = 'none';
        document.getElementById('COI_SCROLL').classList.remove('overflow-hidden');
        document.getElementById('COI_SCROLL').classList.add('overflow-y-scroll');
    }, 500);
}

export function openCommonModal(modalName: string = 'confirmation-modal'): void {
    const triggerBtn = document.getElementById(`${modalName}-trigger-btn`);
    if (triggerBtn) {
        triggerBtn.click();
    }
}

export function closeCommonModal(modalName: string = 'confirmation-modal'): void {
    const closeBtn = document.getElementById(`${modalName}-dismiss-btn`);
    if (closeBtn) {
        closeBtn.click();
    }
}

export function focusElementyById(element_id: string): void {
    const focusElement: HTMLElement | null = document.getElementById(element_id);
    focusElement?.blur();
    focusElement?.focus();
}


export function openCoiSlider(element_id:string): void {
    setTimeout(() => {
        if (element_id) {
            document.getElementById(`${element_id}-trigger-btn`)?.click();
        }
    });
}

export function closeCoiSlider(element_id:string): void {
    setTimeout(() => {
        if (element_id) {
            document.getElementById(`${element_id}-close-btn`)?.click();
        }
    });
}

export function checkForVowelInFirstLetter(word) {
    return ['a', 'e', 'i', 'o', 'u'].includes(word[0].toLowerCase()) ? `an ${word}` : `a ${word}`;
}
