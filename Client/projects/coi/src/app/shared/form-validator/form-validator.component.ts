import { Component, EventEmitter, Input, OnChanges, Output, SimpleChanges } from '@angular/core';
import { Router, NavigationEnd } from '@angular/router';
import { ValidationResponse } from './form-validator.interface';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { fadeIn, slideDown } from './form-validator-animation';

declare const $: any;

@Component({
    selector: 'app-form-validator',
    templateUrl: './form-validator.component.html',
    styleUrls: ['./form-validator.component.scss'],
    animations: [
        slideDown, fadeIn
      ]
})
export class FormValidatorComponent implements OnChanges {

    @Input() validationList: ValidationResponse[] = [];
    @Input() proceedBtnName = 'Submit';
    @Output() proceedAction = new EventEmitter<any>();

    subscription$ = [];
    isShowDock = false;
    currentIndex = 0;
    defaultFormPath = '/coi/opa/form';
    navigationDetails: any = null;
    isErrorPresent = false;
    isShowNavigationIcons = false;
    isModalOpened = false;

    constructor(private _router: Router) {
        // this part invokes on tab change and look for the div id that the user has selected. If the div id is found, then the user is navigated to that part, else reties for 25 times
        this.subscription$.push(this._router.events.subscribe(async (event: any) => {
            if (this.validationList?.length && (event instanceof NavigationEnd)) {
                let RETRY_COUNT = 0;
                do {
                    await new Promise(resolve => setTimeout(() => {
                        this.addValidations();
                        this.scrollToValidation(this.navigationDetails?.id);
                        resolve('');
                    }, 500));
                    if (document.getElementById(this.navigationDetails?.id)) {
                        this.navigationDetails = null;
                    }
                    RETRY_COUNT++;
                } while (!document.getElementById(this.navigationDetails?.id) && RETRY_COUNT <= 25);
            }
        }));
    }

    ngOnChanges(change: SimpleChanges): void {
        this.currentIndex = 0;
        this.clearValidation();
        if (this.validationList?.length) {
            this.addValidations();
            if (!change.validationList.previousValue?.length && change.validationList.currentValue?.length) {
                this.isModalOpened = true;
                $('#validate-form-modal').modal('show');
            }
            this.isErrorPresent = this.validationList.some(e => e.validationType == 'VE' || e.validationType == 'VM');
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.subscription$);
    }


    moveToNextValidation(direction) {
        direction === 'RIGHT' ? this.nextValidation() : this.previousValidation();
    }

    nextValidation() {
        this.currentIndex = this.currentIndex + 1;
        if (this.currentIndex > this.validationList.length - 1) {
            this.currentIndex = 0;
        }
        this.navigateToDiv();
    }

    previousValidation() {
        this.currentIndex = this.currentIndex - 1;
        if (this.currentIndex < 0) {
            this.currentIndex = this.validationList.length - 1;
        }
        this.navigateToDiv();
    }

    private navigateToDiv() {
        const { componentId, navigationURL } = this.validationList[this.currentIndex];
        this.goToCorrespondingValidation(componentId, navigationURL);
    }

    goToCorrespondingValidation(id: string, navigationURL: string = this.defaultFormPath) {
        document.URL.includes(navigationURL) ?
            this.scrollToValidation(id) : this.navigateToDocumentRoutePath(id, navigationURL);
    }

    scrollToValidation = (elementId: string, position: 'start' | 'center' | 'end' | 'nearest' | 'below-header' = 'center') => {
        const ELEMENT: HTMLElement = document.getElementById(elementId);
        if (position === 'below-header') {
            const headerOffset = 250;
            const elementPosition = ELEMENT.getBoundingClientRect().top;
            const offsetPosition = elementPosition + window.pageYOffset - headerOffset;
            return window.scrollTo({ behavior: 'smooth', top: offsetPosition });
        }
        return ELEMENT ? ELEMENT.scrollIntoView({ behavior: 'smooth', block: position }) : false;
    };

    navigateToDocumentRoutePath(id: string, navigationURL: string) {
        this._router.navigate([navigationURL], { queryParamsHandling: 'merge' });
        this.navigationDetails = { id: id, url: navigationURL };
    }

    private addValidations() {
        this.validationList?.forEach((element: ValidationResponse) => {
            this.addValidationHighlightToContent(element);
        });
    }

    private addValidationHighlightToContent(validation: ValidationResponse) {
        const ELEMENT: HTMLElement = document.getElementById(validation?.componentId);
        if (ELEMENT) {
            this.addValidationBorder(ELEMENT, validation?.validationType);
            const isValidationExists = ELEMENT.textContent.includes(validation?.validationMessage)
            if (!isValidationExists) {
                this.addValidationMessage(validation, ELEMENT);
            }
        }
    }

    private addValidationMessage(validation: ValidationResponse, ELEMENT: HTMLElement) {
        const validationTextNode = document.createElement('div');
        validationTextNode.id = 'validation-alert';
        validationTextNode.classList.add('alert', 'd-flex', 'align-items-center', 'py-2', 'mt-3');
        validationTextNode.classList.add(validation?.validationType != 'VW' ? 'alert-danger' : 'alert-warning');
        const validationIcon = document.createElement('i');
        validationIcon.classList.add('fa', 'me-3');
        validationIcon.classList.add(validation?.validationType != 'VW' ? 'fa-exclamation-circle' : 'fa-exclamation-triangle');
        const validationTextDiv = document.createElement('div');
        const validationText = document.createTextNode(validation?.validationMessage);
        validationTextDiv.appendChild(validationText);
        validationTextNode.appendChild(validationIcon);
        validationTextNode.appendChild(validationTextDiv);
        ELEMENT.insertBefore(validationTextNode, ELEMENT.firstChild);
    }

    private addValidationBorder(ELEMENT: HTMLElement, validationType: string) {
        if (!ELEMENT.classList.contains('error-highlight-card')) {
            ELEMENT.classList.add(validationType != 'VW' ? 'error-highlight-card' : 'warning-highlight-card', 'px-3', 'rounded-2');
        }
    }

    clearValidation() {
        const validationContainers = document.querySelectorAll('#validation-alert');
        validationContainers.forEach(element => {
            element.remove();
        });
        const validationErrorHighlights = document.querySelectorAll('.error-highlight-card');
        validationErrorHighlights.forEach(element => {
            element.classList.remove('error-highlight-card', 'px-3', 'rounded-2');
        });
        const validationWarningHighlights = document.querySelectorAll('.warning-highlight-card');
        validationWarningHighlights.forEach(element => {
            element.classList.remove('warning-highlight-card', 'px-3', 'rounded-2');
        });
    }

    emitProceedAction() {
        this.proceedAction.emit('PROCEED');
    }

}
