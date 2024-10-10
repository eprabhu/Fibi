import {ChangeDetectorRef, Component, EventEmitter, Input, OnChanges, OnDestroy, Output} from '@angular/core';
import {forkJoin, Observable, Subscription} from 'rxjs';
import {QuestionnaireListService} from './questionnaire-list.service';
import {Router} from '@angular/router';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import {fileDownloader} from "../../../../../fibi/src/app/common/utilities/custom-utilities";
import {CommonService} from "../../common/services/common.service";
import {HttpClient} from "@angular/common/http";
import {ApplicableQuestionnaire} from '../../disclosure/coi-interface';
import {HTTP_ERROR_STATUS} from '../../app-constants';

interface Configuration {
    moduleItemCode: number;
    moduleSubitemCodes: Array<number>;
    moduleItemKey: string;
    moduleSubItemKey: string;
    actionUserId: string;
    actionPersonName: string;
    enableViewMode: any;
    isChangeWarning: boolean;
    isEnableVersion: boolean;
    questionnaireNumbers: Array<number>;
}

/**
 * Written by Mahesh Sreenath V M
 * this is parent component that handles versioning of Questionnaire
 * its takes 'Configuration" as input and fetches all applicable questionnaires for a given module.
 * moduleItemCode: number = module code for your module check couhes module table for more detail;
 * moduleSubitemCodes: Array<number> = the list of sub modules you want to include;
 * moduleItemKey: string = the item for which you want to fetch the data proposalId awardId etc;
 * moduleSubItemKey: string = the subitem which you want to fetch data for amendment, task etc;
 * actionUserId: string; = the id of the user who performs the action
 * actionPersonName: string; name of the user who performs the action
 * enableViewMode: any =  accepts boolean of array of sub modules for which it should be view mode
 *               true| false | [sub_module1, sub_module2] for given sub modules the questionnaire will be on view mode
 * isEnableVersion: boolean = checks if we should show new version pop up this will be combined with view mode value;
 */

/**
 * Author - Sreejith T
 * @Input() - isAutoSaveEnabled - Whether autosave feature is enabled
 */

@Component({
    selector: 'app-view-questionnaire-list',
    templateUrl: './view-questionnaire-list.component.html',
    styleUrls: ['./view-questionnaire-list.component.scss'],
    providers: [QuestionnaireListService]
})
export class ViewQuestionnaireListComponent implements OnChanges, OnDestroy {

    @Input() configuration: Configuration;
    @Output() QuestionnaireSaveEvent: EventEmitter<boolean> = new EventEmitter<boolean>();
    @Output() QuestionnaireEditEvent: EventEmitter<boolean> = new EventEmitter<boolean>();
    @Input() isShowExportButton = true;
    @Input() externalSaveEvent: Observable<any>;
    @Input() isShowSave = true;
    @Input() isShowQuestionnaireDock = true;
    @Input() questionnaireHeader = 'Questionnaire';
    @Input() isShowBackButton = false;
    @Input() isShowCollapse = false;
    @Input() isAutoSaveEnabled = false;
    @Input() saveButtonLabel = 'Save';
    @Output() currentActiveQuestionnaire: EventEmitter<any> = new EventEmitter<any>();
    @Input() isQuestionnaireValidateMode: boolean = false;

    requestObject = {
        moduleItemCode: null,
        moduleSubItemCode: null,
        moduleSubItemKey: '',
        moduleItemKey: '',
        actionUserId: '',
        actionPersonName: '',
        questionnaireNumbers: null,
        questionnaireMode: null
    };
    isViewMode: boolean;
    questionnaireList: any = [];
    activeQuestionnaire: any = {};
    selectedIndex: number = null;
    tempSelectedIndex: number;
    list: Array<any> = [];
    $subscriptions: Subscription[] = [];
    isSaving = false;
    isShowQuestionnaire = true;
    isShowNoDatacard = false;
    private hasPendingAPIs = false;
    private hasQueuedTabSwitch = false;

    constructor(private _http: HttpClient, private _questionnaireListService: QuestionnaireListService, public _commonService: CommonService,
                private _router: Router, private _CDRef: ChangeDetectorRef) {
    }

    ngOnChanges() {
        if (this.configurationValidation()) {
            this.selectedIndex = null;
            this.loadAllQuestionnaires();
        }
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    updateFlag(event) {
        this.questionnaireList[this.selectedIndex].QUESTIONNAIRE_COMPLETED_FLAG = event;
    }

    configurationValidation() {
        if (this.configuration && !this.configuration.moduleItemCode) {
            return false;
        }
        if (!this.configuration.moduleSubitemCodes.length) {
            return false;
        }
        if (!this.configuration.moduleItemKey) {
            return false;
        }
        if (!this.configuration.actionPersonName && !this.configuration.actionUserId) {
            return false;
        }
        return true;
    }

    /**
     * clears the current questionnaireList and fetches data for each sub module code. since there
     * is chance to combine multiple sub modules for a given module
     */
    loadAllQuestionnaires() {
        this.isShowNoDatacard = false;
        this.questionnaireList = [];
        this.list = [];
        this.configuration.moduleSubitemCodes.forEach(subItemCode => this.setRequestObject(subItemCode));
        this.$subscriptions.push(
            forkJoin(...this.list).subscribe(data => {
                if (data) {
                    this.isShowNoDatacard = true;
                }
                this.questionnaireList = [];
                data.forEach((d: any) => this.combineQuestionnaireList(d.applicableQuestionnaire));
                this.checkQuestionnaireOpened();
                this._CDRef.markForCheck();
                if (this.questionnaireList.length && this.isQuestionnaireValidateMode) {
                    this.validateMandatory();
                }
            }, (_error: any) => {
                this.isShowNoDatacard = true;
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in loading screening questionnaire.');
            }));
    }

    setRequestObject(subItemCode: number): void {
        this.requestObject.moduleItemCode = this.configuration.moduleItemCode;
        this.requestObject.moduleSubItemCode = subItemCode;
        this.requestObject.moduleItemKey = this.configuration.moduleItemKey;
        this.requestObject.moduleSubItemKey = this.configuration.moduleSubItemKey;
        this.requestObject.actionPersonName = this.configuration.actionPersonName;
        this.requestObject.actionUserId = this.configuration.actionUserId;
        this.requestObject.questionnaireMode = this.getQuestionnaireMode(subItemCode);
        this.requestObject.questionnaireNumbers = this.configuration.questionnaireNumbers || [];
        this.list.push(this.getApplicableQuestionnaire(this.requestObject));
    }

    getQuestionnaireMode(subItemCode: number): string {
        const isViewMode = this.checkViewMode(subItemCode);
        return isViewMode ? 'ANSWERED' : 'ACTIVE_ANSWERED_UNANSWERED';
    }

    getApplicableQuestionnaire(requestObject) {
        requestObject = JSON.parse(JSON.stringify(requestObject));
        return this._http.post(this._commonService.fibiUrl + '/getApplicableQuestionnaire', requestObject)
    }

    combineQuestionnaireList(newList) {
        this.questionnaireList = [...this.questionnaireList, ...newList];
    }

    checkQuestionnaireOpened() {
        return (this.selectedIndex === null && !this.isQuestionnaireValidateMode) ? this.versionWarning(0) : false;
    }

    isQuestionnaireChanged(index) {
        this.clearPreviousActiveTabs();
        this.tempSelectedIndex = index;
        if (this.isAutoSaveEnabled) {
            if (this.hasPendingAPIs) {
                this.showLoaderMessageAndQueueTabSwitch();
            } else {
                this.versionWarning(index);
            }
        } else {
            this.configuration.isChangeWarning && this.activeQuestionnaire.isChanged ?
                this.openModalOnDataChange() : this.versionWarning(index);
        }
    }

    /**
     * Function for showing loader and queuing tab switch
     * - the default API loader will be shown with a custom message
     * - tab switch by the user will be placed in a queue till the autosave APIs are completed
     */
    private showLoaderMessageAndQueueTabSwitch(): void {
        this._commonService.isPreventDefaultLoader = false;
        this._commonService.isShowLoader.next(true);
        this._commonService.appLoaderContent = 'Saving... Please wait a moment.';
        this.hasQueuedTabSwitch = true;
    }

    /**
     * Function for updating the hasPendingAPIs flag
     * - hasPendingAPIs flag is used to know whether there are any pending autosave API calls to be finished.
     * - value is true if there is pending API calls and value is false if there are no pending API calls
     * - if hasQueuedTabSwitch is true (there is a tab switching done by user, but placed in queue since API calls are not completed)
     * and hasPendingAPIs is true then we switch the tab since API calls are completed
     */
    public isAPIRequestPendingFlagUpdate(flag: boolean): void {
        this.hasPendingAPIs = flag;
        if (this.hasQueuedTabSwitch && !this.hasPendingAPIs) {
            this.hasQueuedTabSwitch = false;
            this._commonService.isShowLoader.next(false);
            this._commonService.appLoaderContent = 'Loading...';
            this.versionWarning(this.tempSelectedIndex);
        }
    }

    markQuestionnaireAsEdited(status: boolean): void {
        this.QuestionnaireEditEvent.emit(status);
    }

    openModalOnDataChange() {
        document.getElementById('unsavedChangeModalButton').click();
    }

    /**
     * openQuestionnaire - updates the value of currently active questionnaire in activeQuestionnaire
     * @param index
     */
    openQuestionnaire(index) {
        this.setCurrentSelectedIndex(index);
        if (this.questionnaireList.length) {
            this.isViewMode = this.checkViewMode(this.questionnaireList[index].MODULE_SUB_ITEM_CODE);
            this.activeQuestionnaire.isChanged = false;
            this.activeQuestionnaire = Object.assign({}, this.questionnaireList[index]);
            this.currentActiveQuestionnaire.emit(this.activeQuestionnaire);
        } else {
            this.activeQuestionnaire.QUESTIONNAIRE_ANS_HEADER_ID = '';
        }
    }

    /**updateParentData - to update QUESTIONNAIRE_ANS_HEADER_ID and QUESTIONNAIRE_COMPLETED_FLAG of answered questionnaire
     * and also update parent regarding the completion status of all questionnaires
     * @param event
     */
    updateParentData(event) {
        if (event.status === 'SUCCESS') {
            this.questionnaireList[this.selectedIndex].QUESTIONNAIRE_ANS_HEADER_ID = event.data.QUESTIONNAIRE_ANS_HEADER_ID;
            this.questionnaireList[this.selectedIndex].QUESTIONNAIRE_COMPLETED_FLAG = event.data.QUESTIONNAIRE_COMPLETED_FLAG;
            this.questionnaireList[this.selectedIndex].TRIGGER_POST_EVALUATION = event.data.TRIGGER_POST_EVALUATION;
            this.questionnaireList[this.selectedIndex].NEW_QUESTIONNAIRE_ID = event.data.NEW_QUESTIONNAIRE_ID;
            this.questionnaireList[this.selectedIndex].QUESTIONNAIRE_ID = event.data.QUESTIONNAIRE_ID;
            this.questionnaireList[this.selectedIndex].ANS_UPDATE_TIMESTAMP = event.data.ANS_UPDATE_TIMESTAMP;
            const QUESTIONNAIRE = {...this.questionnaireList[this.selectedIndex]};
            QUESTIONNAIRE.IS_ALL_QUESTIONNAIRES_COMPLETE = this.isAllQuestionnairesCompleted();
            this.QuestionnaireSaveEvent.emit(QUESTIONNAIRE);
        } else if (event.status === 'UPDATE') {
            this.questionnaireList[this.selectedIndex].QUESTIONNAIRE_COMPLETED_FLAG = event.data.QUESTIONNAIRE_COMPLETED_FLAG;
        } else {
            this.QuestionnaireSaveEvent.emit(false);
        }
    }

    versionWarning(index) {
        this.setCurrentSelectedIndex(index);
        if (this.configuration.isEnableVersion && this.questionnaireList.length > 0 &&
            this.questionnaireList[index].NEW_QUESTIONNAIRE_ID &&
            !this.checkViewMode(this.questionnaireList[index].MODULE_SUB_ITEM_CODE)) {
            document.getElementById('confirmVersionModalButton').click();
        } else {
            this.openQuestionnaire(this.selectedIndex);
        }
    }

    setCurrentSelectedIndex(index) {
        this.selectedIndex = index;
    }

    /**
     * @param  {} questionnaire
     * for a given questionnaire updates the dat object with new questionnaire values
     */
    updateQuestionnaireVersion(questionnaire) {
        questionnaire.QUESTIONNAIRE_ID = questionnaire.NEW_QUESTIONNAIRE_ID;
        questionnaire.QUESTIONNAIRE_LABEL = questionnaire.NEW_QUESTIONNAIRE_LABEL || questionnaire.QUESTIONNAIRE;
        questionnaire.NEW_QUESTIONNAIRE_ID = questionnaire.NEW_QUESTIONNAIRE_ID;
        questionnaire.QUESTIONNAIRE_ANS_HEADER_ID = questionnaire.QUESTIONNAIRE_ANS_HEADER_ID || '';
        questionnaire.QUESTIONNAIRE_COMPLETED_FLAG = 'N';
        this.openQuestionnaire(this.selectedIndex);
    }

    /**
     * @param  {} subModuleCode
     * @returns boolean
     * checks if the questionnaire should be opened in view or edit mode by identifying the input.
     * also checks if the input is available in given view mode list
     */
    checkViewMode(subModuleCode): boolean {
        return this.configuration.enableViewMode.length ? this.checkInList(subModuleCode) : this.configuration.enableViewMode;
    }

    checkInList(subModuleCode): boolean {
        return this.configuration.enableViewMode.includes(subModuleCode);
    }

    exportAsTypeDoc(docType) {
        if (!this.isSaving) {
            this.isSaving = true;
            const exportDataReqObject = {
                moduleItemKey: this.configuration.moduleItemKey,
                moduleCode: this.configuration.moduleItemCode,
                subModuleCode: this.activeQuestionnaire.MODULE_SUB_ITEM_CODE,
                subModuleItemKey: this.configuration.moduleSubItemKey,
                questionnaireMode: this.getQuestionnaireMode(this.activeQuestionnaire.MODULE_SUB_ITEM_CODE),
                personId: this._commonService.getCurrentUserDetail('personID'),
                exportType: docType === 'excel' ? 'xlsx' : docType === 'pdf' ? 'pdf' : '',
                userName: this._commonService.getCurrentUserDetail('userName'),
                questionnaireId: this.activeQuestionnaire.QUESTIONNAIRE_ID,
                isSingleQuestionnairePrint: false
            };
            this.exportFileName(exportDataReqObject);
        }
    }

    exportFileName(exportDataReqObject) {
        this.$subscriptions.push(this._questionnaireListService.generateQuestionnaireReport(exportDataReqObject).subscribe(
            data => {
                const fileName = 'Questionnaires' + '_' + this.configuration.moduleItemKey;
                // msSaveOrOpenBlob only available for IE & Edge
                fileDownloader(data.body, fileName, exportDataReqObject.exportType);
                this.isSaving = false;
            }, err => {
                this.isSaving = false;
            }));
    }

    backToAgreementList() {
        this._router.navigate(['fibi/dashboard/agreementsList']);
    }

    /** this function returns true if all questionnaire listed in Questionnaire list is complete
     * The new questionnaireID flag is checked because even though if a questionnaire is complete there is
     * a chance that a new version is available if that is the case then the questionnaire is
     * considered as incomplete until user saves it(with all answers completed).
     * The idea is that the new version will have updated questionnaire.
     */
    isAllQuestionnairesCompleted(): boolean {
        return this.questionnaireList.every(Q => Q.QUESTIONNAIRE_COMPLETED_FLAG === 'Y' && !Q.NEW_QUESTIONNAIRE_ID);
    }

    validateMandatory(): any {
        let unAnsweredMandatoryQuestionnaire = this.getMandatoryNotCompletedQuestionnaire(this.questionnaireList);
        if (unAnsweredMandatoryQuestionnaire && unAnsweredMandatoryQuestionnaire?.QUESTIONNAIRE_ID) {
            const questionnaireIndex = this.getIndexFromQuestionnaireId(this.questionnaireList, unAnsweredMandatoryQuestionnaire?.QUESTIONNAIRE_ID);
            this.versionWarning(questionnaireIndex);
            setTimeout(() => {
                document.getElementById(`${unAnsweredMandatoryQuestionnaire?.QUESTIONNAIRE_ID}`).classList.add('active');
            }, 10);
        }

    }

    getMandatoryNotCompletedQuestionnaire(questionnaires: ApplicableQuestionnaire[]) {
        return questionnaires.find((element) => element.IS_MANDATORY === 'Y' && element.QUESTIONNAIRE_COMPLETED_FLAG !== 'Y');
    }

    clearPreviousActiveTabs() {
        const navTabList = document.querySelectorAll('.subTabLink');
        navTabList.forEach(navTabEl => {
            navTabEl.classList.remove('active');
        });
    }

    getIndexFromQuestionnaireId(questionnaireList: any[], id) {
        return questionnaireList.findIndex(obj => obj.QUESTIONNAIRE_ID === id);
    }
}
