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

    setTimeout(() => {
        document.getElementById(sliderName).classList.add('slider-opened');
    });
}

export function closeSlider(sliderName: string = 'coi-slider'): void {
    document.getElementById(sliderName).classList.remove('slider-opened');

    setTimeout(() => {
        document.getElementById(`${sliderName}-overlay`).style.display = 'none';
    }, 500);
}

export function focusElementById(element_id: string): void {
    const focusElement: HTMLElement | null = document.getElementById(element_id);
    focusElement?.blur();
    focusElement?.focus();
}


export function openCoiSlider(element_id: string): void {
    setTimeout(() => {
        if (element_id) {
            document.getElementById(`${element_id}-trigger-btn`)?.click();
        }
    });
}

export function closeCoiSlider(element_id: string): void {
    setTimeout(() => {
        if (element_id) {
            document.getElementById(`${element_id}-close-btn`)?.click();
        }
    });
}

export function checkForVowelInFirstLetter(word) {
    return ['a', 'e', 'i', 'o', 'u'].includes(word[0].toLowerCase()) ? `an ${word}` : `a ${word}`;
}

/**
 * Scrolls the page to a specified section by its ID, with an optional offset for the header.
 *
 * @param sectionId - The ID of the section to scroll to.
 * @param offsetTop - The offset top, which is subtracted from the scroll position.
 */
export function jumpToSection(sectionId = '', offset = 0) {
    const SECTION_ELEMENT: HTMLElement | null = document.getElementById(sectionId);
    // Scroll the element into view
    SECTION_ELEMENT.scrollIntoView({ behavior: 'smooth', block: 'center' });

    // Calculate the new scroll position
    const elementRect = SECTION_ELEMENT.getBoundingClientRect();
    const offsetTop = window.pageYOffset + elementRect.top - offset;

    // Apply the scroll offset
    window.scrollTo({
        top: offsetTop,
        behavior: 'smooth'
    });
}

export function getFormattedSponsor(sponsorCode: any, sponsorName: any): string {
    return sponsorCode && sponsorName ? `${sponsorCode} - ${sponsorName}` : sponsorCode || sponsorName;
}

/**
 * Opens a common modal dialog by triggering its associated button click event.
 *
 * @param modalName - The name of the modal, used to construct the trigger button's ID.
 */
export function openCommonModal(modalName: string): void {
    const triggerBtn = document.getElementById(`${modalName}-trigger-btn`);
    if (triggerBtn) {
        triggerBtn.click();
        focusElementById(modalName);
    }
}

/**
 * Closes a common modal dialog by triggering its associated close button click event.
 *
 * @param modalName - The name of the modal, used to construct the close button's ID.
 */
export function closeCommonModal(modalName: string = 'common-modal'): void {
    const closeBtn = document.getElementById(`${modalName}-dismiss-btn`);
    if (closeBtn) {
        closeBtn.click();
    }
}

export function scrollToElementWithinBoundaries(focusElement: HTMLElement | undefined, boundaryTop = 0, boundaryBottom = 0, scrollableElement: HTMLElement | any = window) {
    if (focusElement) {
        const elementRect = focusElement.getBoundingClientRect();
        const scrollableRect = scrollableElement === window ? { top: 0, bottom: window.innerHeight } : scrollableElement.getBoundingClientRect();
        const scrollTop = scrollableElement === window ? window.pageYOffset : scrollableElement.scrollTop;

        const absoluteElementTop = elementRect.top + scrollTop - scrollableRect.top;
        const elementBottom = elementRect.bottom + scrollTop - scrollableRect.top;

        const scrollableTop = scrollTop + boundaryTop;
        const scrollableBottom = scrollTop + (scrollableElement === window ? window.innerHeight : scrollableElement.clientHeight) - boundaryBottom;

        if (absoluteElementTop < scrollableTop) {
            scrollableElement.scrollTo({
                top: absoluteElementTop - boundaryTop,
                behavior: 'smooth'
            });
        } else if (elementBottom > scrollableBottom) {
            scrollableElement.scrollTo({
                top: elementBottom - (scrollableElement === window ? window.innerHeight : scrollableElement.clientHeight) + boundaryBottom,
                behavior: 'smooth'
            });
        } else {
            focusElement.scrollIntoView({
                block: 'nearest',
                inline: 'nearest',
                behavior: 'smooth'
            });
        }
    }
}

export function isExistSearchWord (object: any, searchText: string, searchKeys: Array<string | string[]>, needFullCheck: boolean): boolean {
    // Check for combined key values with delimiters first
    for (const key of searchKeys) {
        if (typeof key === 'string' && key.startsWith('[') && key.endsWith(']')) {
            const keyCombination = key.slice(1, -1); // Remove the brackets
            const delimiter = keyCombination.includes(' - ') ? ' - ' :
                              keyCombination.includes(':') ? ':' : ' ';
            const keys = keyCombination.split(delimiter).map(k => k.trim());
            const combinedValue = keys.map(k => object[k] ? object[k].toString() : '').join(delimiter);
            if (doesValueMatchSearchText(combinedValue, searchText)) {
                return true;
            }
        } else {
            // Handle individual keys
            const value = object[key as string];
            if (value && doesValueMatchSearchText(value, searchText)) {
                return true;
            }
        }
    }

    // If the value is an object, recursively check its properties
    for (const key in object) {
        const value = object[key];
        if (needFullCheck && value && typeof value === 'object' && !Array.isArray(value)) {
            if (isExistSearchWord(value, searchText, searchKeys, needFullCheck)) {
                return true;
            }
        }

        // If the value is an array, check each item in the array
        if (needFullCheck && Array.isArray(value)) {
            for (const item of value) {
                if (isExistSearchWord(item, searchText, searchKeys, needFullCheck)) {
                    return true;
                }
            }
        }
    }

    return false;
}

export function doesValueMatchSearchText(value: any, searchText: string): boolean {
    // Check if the current value contains the search text
    if (typeof value === 'string' || typeof value === 'number') {
        return value.toString().trim().toLowerCase().includes(searchText.trim().toLowerCase());
    }
    return false;
}

/*
 * checks whether a given email address is valid or not.
 * @param emailAddress
 */
export function isValidEmailAddress(emailAddress: string): boolean {
    // tslint:disable-next-line: max-line-length
    const emailRgx = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
    return emailRgx.test(String(emailAddress).trim().toLowerCase());
}

export function phoneNumberValidation(input: any) {
    const pattern = (/^(?:(?:\(?(?:00|\+)([1-4]\d\d|[1-9]\d?)\)?)?[0-9]?)?((?:\(?\d{1,}\)?[\-\.\ \\\/]?){0,})(?:[\-\.\ \\\/]?(?:#|ext\.?|extension|x)[\-\.\ \\\/]?(\d+))?$/);
    if (!pattern.test(input)) {
        const newPattern = /^([a-zA-Z]|[0-9a-zA-Z])+$/;
        return (newPattern.test(input)) ? 'Alphabets cannot be added in  Phone number field.' : ' Please provide a valid phone number';
    } else {
        return null;
    }
}

export function inputRestrictionForNumberField(input: any) {
    const pattern = /[0-9\+\-\/\ ]/;
    if (!pattern.test(input)) {
        return true;
    } else {
        return null;
    }
}

/**
 * check if an object is having no key value pairs.
 * checks explicitly if the given variable is of type Object.
 * @param obj
 */
export function isEmptyObject(obj: any): boolean {
    return obj && Object.keys(obj).length === 0 && Object.getPrototypeOf(obj) === Object.prototype;
}


