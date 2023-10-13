import {Component, OnInit} from '@angular/core';
import {FormBuilderEvent} from '../shared/form-builder-view/form-builder-interface';
import {Subject} from 'rxjs';
import {OpaService} from './services/opa.service';
import {isEmptyObject} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import {DataStoreService} from './services/data-store.service';
import {CommonService} from '../common/services/common.service';
import {environment} from '../../environments/environment';
import {HOME_URL, HTTP_ERROR_STATUS} from '../app-constants';
import {OpaDisclosure} from './opa-interface';
import {DefaultAssignAdminDetails, PersonProjectOrEntity} from '../shared-components/shared-interface';
import {HTTP_SUCCESS_STATUS} from '../../../../fibi/src/app/app-constants';
import {Router} from '@angular/router';

@Component({
    selector: 'app-opa',
    templateUrl: './opa.component.html',
    styleUrls: ['./opa.component.scss']
})
export class OpaComponent implements OnInit {
    isCardExpanded = true;
    formBuilderEvents = new Subject<FormBuilderEvent>();
    opaData: OpaDisclosure = new OpaDisclosure();
    deployMap = environment.deployUrl;
    isAddAssignModalOpen = false;
    defaultAdminDetails = new DefaultAssignAdminDetails();
    personProjectDetails = new PersonProjectOrEntity();
    helpTexts = [];
    primaryBtnName = '';
    descriptionErrorMsg = '';
    textAreaLabelName = '';
    withdrawErrorMsg = 'Describe the reason for withdrawing the disclosure';
    returnErrorMsg = 'Describe the reason for returning the disclosure';
    withdrawHelpTexts = [
        `Withdraw any disclosure in 'Submitted' status.`,
        `Describe the reason for withdrawal in the field provided.`,
        `Click on 'Withdraw' button to recall your disclosure for any modification.`
    ];
    returnHelpTexts = [
        `Return any disclosure in 'Review in progress' status.`,
        `Describe the reason for returning  in the field provided.`,
        `Click on 'Return' button to return the disclosure for any modification.`
    ];
    description: any;
    showPersonDetailsModal = false;
    personDetailsModalVO = {personId: '', fullName: ''};
    $subscriptions = [];

    constructor(private _opa: OpaService,
                private _router: Router,
                public commonService: CommonService,
                private dataStore: DataStoreService) {
    }

    ngOnInit(): void {
        this.getDataFromStore();
    }

    triggerSave() {
        this._opa.formBuilderEvents.next({eventType: 'SAVE'});
    }

    submitOPA() {
        this.$subscriptions.push(this._opa.submitOPA(this.opaData.opaDisclosureId, this.opaData.opaDisclosureNumber)
            .subscribe((res: any) => {
                this.opaData = res;
                this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opaData});
            }, err => this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')));
    }

    getManageDisclosureRight(): boolean {
        return this.commonService.getAvailableRight('MANAGE_FCOI_DISCLOSURE') || true;
    }

    openAddAssignModal(): void {
        this.isAddAssignModalOpen = true;
        this.setAssignAdminModalDetails();
    }

    closeAssignAdministratorModal(event) {
        if (event && (event.adminPersonId || event.adminGroupId)) {
            // this.getCoiReview();
            this.opaData = event;
            this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opaData});
        }
        this.isAddAssignModalOpen = false;
    }

    openConfirmationModal(actionBtnName: string, helpTexts: string [] = [], descriptionErrorMsg: string = ''): void {
        this.helpTexts = helpTexts;
        this.primaryBtnName = actionBtnName;
        this.descriptionErrorMsg = descriptionErrorMsg;
        this.textAreaLabelName = actionBtnName === 'Withdraw' ? ' Withdrawal' : 'Return';
        this.setPersonProjectDetails();
        document.getElementById('disclosure-confirmation-modal-trigger-btn').click();
    }

    performDisclosureAction(event): void {
        this.description = event;
        switch (this.primaryBtnName) {
            case 'Return':
                return this.returnDisclosure();
            case 'Withdraw':
                return this.withdrawDisclosure();
            default:
                return;
        }
    }

    returnDisclosure() {
        this.$subscriptions.push(this._opa
            .returnOPA(this.opaData.opaDisclosureId, this.opaData.opaDisclosureNumber)
            .subscribe((res: any) => {
                this.opaData = res;
                this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opaData});
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `OPA returned successfully.`);
                this.goToHomeUrl();
            }, _err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, `Error in returning OPA.`);
            }));
    }

    goToHomeUrl() {
        // TODO admin/reviewer/pi based redirect once rights are implemented.
        const reRouteUrl = this._opa.previousHomeUrl || HOME_URL;
        this._router.navigate([reRouteUrl]);
    }

    withdrawDisclosure() {
        this.$subscriptions.push(this._opa
            .withdrawOPA(this.opaData.opaDisclosureId, this.opaData.opaDisclosureNumber)
            .subscribe((res: any) => {
                this.opaData = res;
                this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opaData});
            }, _err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, `Error in withdrawing disclosure.`);
            }));
    }

    closePersonDetailsModal(event) {
        this.showPersonDetailsModal = event;
    }

    openDetailModal(): void {
        this.personDetailsModalVO.personId = this.opaData.personId;
        this.personDetailsModalVO.fullName = this.opaData.personName;
        this.showPersonDetailsModal = true;
    }

    private getDataFromStore() {
        const opaData = this.dataStore.getData();
        console.log(opaData);
        if (isEmptyObject(opaData)) {
            return;
        }
        this.opaData = opaData.opaDisclosure;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private setAssignAdminModalDetails(): void {
        this.defaultAdminDetails.adminGroupId = this.opaData.adminGroupId;
        this.defaultAdminDetails.adminGroupName = this.opaData.adminGroupName;
        this.defaultAdminDetails.adminPersonId = this.opaData.adminPersonId;
        this.defaultAdminDetails.adminPersonName = this.opaData.adminPersonName;
    }

    private setPersonProjectDetails(): void {
        this.personProjectDetails.personFullName = this.opaData.opaPerson.personName;
        // this.personProjectDetails.projectDetails = this.coiData?.projectDetail;
        this.personProjectDetails.unitDetails = this.opaData.homeUnitName;
    }

    completeDisclosureReview() {
        this.$subscriptions.push(this._opa
            .completeOPAReview(this.opaData.opaDisclosureId, this.opaData.opaDisclosureNumber)
            .subscribe((res: any) => {
                this.opaData = res;
                this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opaData});
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
            }, _err => {
                // if (_err.error.text === 'REVIEW_STATUS_NOT_COMPLETE') {
                //     document.getElementById('reviewPendingCompleteReviewErrorModalTrigger').click();
                // } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in completing review.`);
                // }
            }));
    }

}
