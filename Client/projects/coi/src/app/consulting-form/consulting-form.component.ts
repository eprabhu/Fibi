import { Component, HostListener } from '@angular/core';
import { DataStoreService } from './services/data-store.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../common/services/common.service';
import { ConsultingService } from './services/consulting-service.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS, REPORTER_HOME_URL } from '../app-constants';
import { ActivatedRoute, Router } from '@angular/router';
import { DefaultAssignAdminDetails, DisclsoureHeaderDetails, PersonProjectOrEntity } from '../shared-components/shared-interface';
import { Subject } from 'rxjs';
import { Location } from '@angular/common';
import { FormBuilderEvent } from '../shared/form-builder-view/form-builder-interface';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { NavigationService } from "../common/services/navigation.service";
import { ConsultingForm } from './consulting-form.interface';
import { openCommonModal } from '../common/utilities/custom-utilities';

@Component({
    selector: 'app-consulting-form',
    templateUrl: './consulting-form.component.html',
    styleUrls: ['./consulting-form.component.scss']
})
export class ConsultingFormComponent {

    consultingForm: ConsultingForm = new ConsultingForm();
    $subscriptions = [];
    showPersonDetailsModal = false;
    personDetailsModalVO = { personId: '', fullName: '' };
    showSlider = false;
    selectedType: string;
    isCardExpanded = true;
    isUserCollapse = false;
    isHomeClicked = false;
    isSubmitClicked = false;
    isAddAssignModalOpen = false;
    defaultAdminDetails = new DefaultAssignAdminDetails();
    personProjectDetails = new PersonProjectOrEntity();
    helpTexts = '';
    primaryBtnName = '';
    descriptionErrorMsg = '';
    textAreaLabelName = '';
    returnHelpTexts = 'Please provide the reason for return.';
    withdrawHelpTexts = 'Please provide the reason for withdraw.';
    returnErrorMsg = 'Please provide the reason for returning.';
    withdrawErrorMsg = 'Please provide the reason for withdrawing.';
    returnModalHelpText = 'You are about to return the consulting disclosure.';
    withdrawModalHelpText = 'You are about to withdraw the consulting disclosure.';
    completeReviewHelpText = 'You are about to complete the disclosure\'s final review.';
    submitHelpTexts = 'You are about to submit the consulting disclosure.';
    confirmationHelpTexts = '';
    description: any;
    disclosureHeaderDetails = new DisclsoureHeaderDetails();
    validationList = [];
    formBuilderEvents = new Subject<FormBuilderEvent>();

    constructor(public dataStore: DataStoreService, public commonService: CommonService,
        public location: Location, private _navigationService: NavigationService,
        public consultingService: ConsultingService, private _router: Router,
        private _activatedRoute: ActivatedRoute) { }

    ngOnInit() {
        this.consultingService.headerEntityName = '';
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setTopDynamically();
        this.subscibeFormCompletion();
        this.listenQueryParamsChanges();
    }

    private getDataFromStore(): void {
        this.consultingForm = this.dataStore.getData();
        this.setPersonProjectDetails();
    }

    private listenQueryParamsChanges(): void {
        this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            if (this.consultingForm.consultingFormDisclosure.disclosureId != MODULE_ID) {
               this.consultingService.headerEntityName = '';
               this.validationList = [];
               this.loadNewFormAndUpdateStore(MODULE_ID);
               this.resetChangeFlags();
            }
        }));
    }

    resetChangeFlags() {
        this.consultingService.isFormBuilderDataChangePresent = false;
        this.consultingService.isDataChangeAvailableInEntity = false;
    }

    loadNewFormAndUpdateStore(MODULE_ID: number) {
        this.$subscriptions.push(this.consultingService.loadConsultingFormHeader(MODULE_ID).subscribe((data: any) => {
            this.dataStore.updateStore(['consultingFormDisclosure'], { 'consultingFormDisclosure': data });
        }));
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    openPersonDetailsModal(): void {
        this.personDetailsModalVO.personId = this.consultingForm.consultingFormDisclosure.person.personId;
        this.personDetailsModalVO.fullName = this.consultingForm.consultingFormDisclosure.person.fullName;
        this.showPersonDetailsModal = true;
    }

    openSlider(type: string, count: number): void {
        if (count) {
            this.showSlider = true;
            this.selectedType = type;
        }
    }

    closeHeaderSlider(): void {
        this.showSlider = false;
        this.selectedType = '';
    }

    collapseHeader(): void {
        this.isCardExpanded = !this.isCardExpanded;
        this.isUserCollapse = !this.isUserCollapse;
        this.setTopDynamically();
    }

    setTopDynamically(): void {
        setTimeout(() => {
            const STICKY_HEADER = document.querySelector<HTMLElement>('.header-sticky');
            if (STICKY_HEADER) {
                const elements = document.querySelectorAll('.form-builder-sticky-header');
                if (elements.length) {
                    elements.forEach((element: HTMLElement) => {
                        element.style.top = STICKY_HEADER.offsetHeight + 50 + 'px';
                    });
                }
                const TABLE_STICKY_HEADER = document.querySelectorAll('.form-builder-table-header');
                if (TABLE_STICKY_HEADER.length) {
                    TABLE_STICKY_HEADER.forEach((element: HTMLElement) => {
                        element.style.top = STICKY_HEADER.offsetHeight + 101 + 'px';
                    });
                }
            }
        }, 1000);
    }

    @HostListener('window:resize', ['$event'])
    listenScreenSize() {
        if (!this.isUserCollapse) {
            this.isCardExpanded = window.innerWidth > 1399;
        }
        this.setTopDynamically();
    }

    goToHomeUrl(): void {
        // TODO admin/reviewer/pi based redirect once rights are implemented.
        this.isHomeClicked = true;
        const reRouteUrl = this.consultingService.previousHomeUrl || REPORTER_HOME_URL;
        this._router.navigate([reRouteUrl]);
    }

    triggerSave(): void {
        this.isSubmitClicked = false;
        this.consultingService.formBuilderEvents.next({ eventType: 'SAVE' });
        this.consultingService.globalSave$.next(true);
    }

    consultingSubmissionModal(): void {
        this.isSubmitClicked = true;
        if (this.consultingService.isDataChangeAvailableInEntity) {
            this.consultingService.globalSave$.next(true);
        }
        if (this.consultingService.isFormBuilderDataChangePresent) {
            this.consultingService.formBuilderEvents.next({ eventType: 'SAVE' });
        } else {
            this.validationList = [];
            this.validateForm();
        }
    }

    private validateForm(): void {
        this.$subscriptions.push(this.consultingService.validateForm({
            formBuilderIds: this.consultingForm.consultingFormDisclosure.consultingDisclFormBuilderDetails.map(e => e.formBuilderId),
            moduleItemCode: '27',
            moduleSubItemCode: '0',
            moduleItemKey: this.consultingForm.consultingFormDisclosure.disclosureId.toString(),
            moduleSubItemKey: '0',
        }).subscribe((data: any) => {
            this.validationList = data;
            if (this.validationList.length) {
                this.validationList.map(ele => ele.navigationURL = '/coi/consulting/form');
            }
            if (!this.validationList.length && this.isSubmitClicked) {
                openModal('consulting-submit-confirm-modal');
            }
        }, err => {
            this.commonService.showToast(HTTP_ERROR_STATUS, `Error occurred during from validation.`);
        }));
    }

    private subscibeFormCompletion(): void {
        this.$subscriptions.push(this.consultingService.triggerSaveComplete.subscribe((data: any) => {
            if(this.isSubmitClicked) {
                this.validationList = [];
            }
            if (data && (this.validationList?.length || this.isSubmitClicked)) {
                this.validateForm();
            }
        }));
    }

    checkForConsultingAdmin(): boolean {
        return this.commonService.getAvailableRight(['MANAGE_CONSULTING_DISCLOSURE']);
    }

    isLoggedInUser(personId: string): boolean {
        return this.commonService?.getCurrentUserDetail('personId') === personId;
    }

    openAddAssignModal(): void {
        this.isAddAssignModalOpen = true;
        this.setAssignAdminModalDetails();
    }

    closeAssignAdministratorModal(event: any): void {
        if (event && (event.adminPersonId || event.adminGroupId)) {
            // this.getCoiReview();
            this.consultingForm.consultingFormDisclosure = event;
            this.dataStore.updateStore(['consultingFormDisclosure'], this.consultingForm);
        }
        this.isAddAssignModalOpen = false;
    }

    private setAssignAdminModalDetails(): void {
        this.defaultAdminDetails.adminGroupId = this.consultingForm.consultingFormDisclosure.adminGroupId;
        this.defaultAdminDetails.adminGroupName = this.consultingForm.consultingFormDisclosure.adminGroupName;
        this.defaultAdminDetails.adminPersonId = this.consultingForm.consultingFormDisclosure.adminPersonId;
        this.defaultAdminDetails.adminPersonName = this.consultingForm.consultingFormDisclosure.adminPersonName;
    }

    private setPersonProjectDetails(): void {
        this.personProjectDetails.personFullName = this.consultingForm.consultingFormDisclosure.person.fullName;
        this.personProjectDetails.unitNumber = this.consultingForm.consultingFormDisclosure.homeUnit;
        this.personProjectDetails.unitName = this.consultingForm.consultingFormDisclosure.homeUnitName;
        this.personProjectDetails.homeUnit = this.consultingForm.consultingFormDisclosure.homeUnit;
        this.personProjectDetails.homeUnitName = this.consultingForm.consultingFormDisclosure.homeUnitName;
        this.personProjectDetails.entityName = this.consultingForm.consultingFormDisclosure.personEntity ? this.consultingForm.consultingFormDisclosure.personEntity.coiEntity.entityName : '';
        this.disclosureHeaderDetails.personEmail = this.consultingForm.consultingFormDisclosure.person.emailAddress;
        this.disclosureHeaderDetails.personPrimaryTitle = this.consultingForm.consultingFormDisclosure.person.directoryTitle;
    }

    submitConsultingForm(): void {
        this.$subscriptions.push(this.consultingService.submitConsulting(this.consultingForm.consultingFormDisclosure.disclosureId)
            .subscribe((res: any) => {
                this.consultingForm.consultingFormDisclosure = res;
                this.isSubmitClicked = false;
                this.dataStore.updateStore(['consultingFormDisclosure'], { consultingFormDisclosure: this.consultingForm.consultingFormDisclosure });
                this.resetChangeFlags();
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Disclsousre submitted successfully.`);
            }, err => {
                this.isSubmitClicked = false;
                if (err.status === 405) {
                    this.consultingService.concurrentUpdateAction = 'Submit Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.')
                }
            }));
    }

    openConfirmationModal(actionBtnName: string, helpTexts: string = '', descriptionErrorMsg: string = '', modalHelpText: string = ''): void {
        this.primaryBtnName = actionBtnName;
        this.descriptionErrorMsg = descriptionErrorMsg;
        this.textAreaLabelName = actionBtnName === 'Withdraw' ? ' Withdrawal' : 'Return';
        this.setPersonProjectDetails();
        this.confirmationHelpTexts = '';
        this.helpTexts = '';
        setTimeout(() => {
            this.confirmationHelpTexts = modalHelpText;
            this.helpTexts = helpTexts;
            openCommonModal('consulting-confirmation-modal');
        });
    }

    performDisclosureAction(event: any): void {
        this.description = event;
        switch (this.primaryBtnName) {
            case 'Return':
                return this.returnDisclosure(event);
            case 'Withdraw':
                return this.withdrawDisclosure(event);
            default:
                return;
        }
    }

    private returnDisclosure(event: any): void {
        this.$subscriptions.push(this.consultingService
            .returnConsulting(this.getRequestObj(event))
            .subscribe((res: any) => {
                this.consultingForm.consultingFormDisclosure = res;
                this.dataStore.updateStore(['consultingFormDisclosure'], { consultingFormDisclosure: this.consultingForm.consultingFormDisclosure });
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Consulting Form returned successfully.`);
                this.goToHomeUrl();
            }, err => {
                if (err.status === 405) {
                    this.consultingService.concurrentUpdateAction = 'Return Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in returning disclosure.`);
                }
            }));
    }

    private withdrawDisclosure(event: any): void {
        this.$subscriptions.push(this.consultingService
            .withdrawConsulting(this.getRequestObj(event))
            .subscribe((res: any) => {
                this.consultingForm.consultingFormDisclosure = res;
                this.dataStore.updateStore(['consultingFormDisclosure'], { consultingFormDisclosure: this.consultingForm.consultingFormDisclosure });
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Consulting Form withdrawn successfully.`);
            }, err => {
                if (err.status === 405) {
                    this.consultingService.concurrentUpdateAction = 'Withdraw Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in withdrawing disclosure.`);
                }
            }));
    }

    private getRequestObj(description: string) : any{
        return {
            'disclosureId': this.consultingForm.consultingFormDisclosure.disclosureId,
            'comment': description
        }
    }

    completeDisclosureReview(): void {
        this.$subscriptions.push(this.consultingService
            .completeFinalReview(this.consultingForm.consultingFormDisclosure.disclosureId)
            .subscribe((res: any) => {
                this.consultingForm.consultingFormDisclosure = res;
                this.dataStore.updateStore(['consultingFormDisclosure'], { consultingFormDisclosure: this.consultingForm.consultingFormDisclosure });
            }, err => {
                if (err.status === 405) {
                    this.consultingService.concurrentUpdateAction = 'Complete Final Review';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in completing review.`);
                }
            }));
    }

    cancelConcurrency() : void{
        this.consultingService.concurrentUpdateAction = '';
    }

    leavePageClicked() : void{
        this.consultingService.isFormBuilderDataChangePresent = false;
        this.consultingService.isDataChangeAvailableInEntity = false;
        if (this.isHomeClicked) {
            this.goToHomeUrl();
        } else {
            this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
        }
    }

    closePersonDetailsModal(event: any): void {
        this.showPersonDetailsModal = event;
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
