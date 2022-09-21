import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';

import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { CoiDisclosure } from '../coi-interface';
import { CoiService } from '../services/coi.service';
import { DataStoreService } from '../services/data-store.service';

@Component({
    selector: 'app-screening-questionnaire',
    template: `
        <div>
            <app-view-questionnaire-list [configuration] = "configuration"
                [questionnaireHeader]="''"
                [saveButtonLabel]="'Save and Continue'"
                (QuestionnaireSaveEvent)= "getSaveEvent($event)">
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
        actionUserId: this._commonService.getCurrentUserDetail('personID'),
        actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
        enableViewMode: false,
        isChangeWarning: true,
        isEnableVersion: true,
    };

    constructor(
        private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _router: Router,
        private _coiService: CoiService
    ) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getDataFromStore();
                }
            })
        );
    }

    private getDataFromStore() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.coiDisclosure = DATA.coiDisclosure;
        this.configuration.moduleItemKey = DATA.coiDisclosure.disclosureId;
        this.configuration.enableViewMode = !this._dataStore.getEditModeForCOI();
        Object.assign({}, this.configuration);
    }

    getSaveEvent(_event: any) {
        this.evaluateDisclosureQuestionnaire();
    }

    private evaluateDisclosureQuestionnaire() {
        this.$subscriptions.push(
            this._coiService.evaluateDisclosureQuestionnaire({
                moduleCode : this.configuration.moduleItemCode,
                submoduleCode : 0,
                moduleItemId : this.configuration.moduleItemKey
            }).subscribe((data: boolean) => {
                this.coiDisclosure.isDisclosureQuestionnaire = data;
                this._dataStore.updateStore(['coiDisclosure'], this);
                const NEXT_STEP = data ? '/fibi/coi/sfi' : '/fibi/coi/certify';
                this._router.navigate([NEXT_STEP], { queryParamsHandling: 'preserve' });
                this._coiService.stepTabName = data ? 'sfi' : 'certify';
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in evaluating disclosure.');
            })
        );
    }

}
