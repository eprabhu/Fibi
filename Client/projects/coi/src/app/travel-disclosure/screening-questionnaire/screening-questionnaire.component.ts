import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { deepCloneObject } from "../../../../../fibi/src/app/common/utilities/custom-utilities";
import { HTTP_ERROR_STATUS } from "../../../../../fibi/src/app/app-constants";
import { CoiDisclosure } from '../../disclosure/coi-interface';
import { CoiService } from '../../disclosure/services/coi.service';
import { DataStoreService } from '../../disclosure/services/data-store.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';

@Component({
    selector: 'app-screening-questionnaire',
    template: `
        <div id="screening-questionnaire-coi">
            <app-view-questionnaire-list
                    [isShowExportButton]="false"
                    [configuration]="configuration"
                    [externalSaveEvent]='coiService.globalSave$'
                    [questionnaireHeader]="''"
                    [isShowSave]="false"
                    [saveButtonLabel]="'Save and Continue'"
                    (QuestionnaireSaveEvent)="getSaveEvent($event)"
                    (QuestionnaireEditEvent) = "triggerConfirmationModal($event)">
            </app-view-questionnaire-list>
        </div>
    `
})
export class ScreeningQuestionnaireComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure'];
    coiDisclosure: CoiDisclosure = new CoiDisclosure();
    configuration: any = {
        moduleItemCode: 8,
        moduleSubitemCodes: [0],
        moduleItemKey: '',
        moduleSubItemKey: 0,
        actionUserId: this._commonService.getCurrentUserDetail('personId'),
        actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
        enableViewMode: false,
        isChangeWarning: true,
        isEnableVersion: true,
    };

    constructor(private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _sfiService: SfiService,
        private _router: Router,
        public coiService: CoiService) { }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getDataFromStore();
                }
            })
        );
    }

    private getDataFromStore(): void {
        const DATA = this._dataStore.getData(this.dependencies);
        this.coiDisclosure = DATA.coiDisclosure;
        this.configuration.moduleItemKey = DATA.coiDisclosure?.disclosureId;
        this.configuration.enableViewMode = !this._dataStore.getEditModeForCOI();
        this.configuration = deepCloneObject(this.configuration);
    }

    getSaveEvent(_event: any): void {
        this._dataStore.dataChanged = false;
        this.coiService.unSavedModules = '';
        if (_event.QUESTIONNAIRE_COMPLETED_FLAG == 'Y') {
            this.evaluateDisclosureQuestionnaire();
        }
    }

    private evaluateDisclosureQuestionnaire(): void {
        this.$subscriptions.push(
            this.coiService.evaluateDisclosureQuestionnaire({
                moduleCode: this.configuration.moduleItemCode,
                submoduleCode: 0,
                moduleItemId: this.configuration.moduleItemKey
            }).subscribe((data: boolean) => {
                this.coiDisclosure.isDisclosureQuestionnaire = data;
                this._sfiService.isSFIRequired = data;
                document.getElementById('questionnaireEvaluationMessageModalTrigger').click();
                this._dataStore.updateStore(['coiDisclosure'], this);
                const NEXT_STEP = data ? '/coi/create-disclosure/sfi' : '/coi/create-disclosure/certification';
                this._router.navigate([NEXT_STEP], { queryParamsHandling: 'preserve' });
                this.coiService.stepTabName = data ? 'sfi' : 'certify';
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in evaluating disclosure.');
            })
        );
    }

    triggerConfirmationModal(changeStatus: boolean): void {
        this.coiService.unSavedModules = 'Screening Questionnaire';
        this._dataStore.dataChanged = changeStatus;
    }

}
