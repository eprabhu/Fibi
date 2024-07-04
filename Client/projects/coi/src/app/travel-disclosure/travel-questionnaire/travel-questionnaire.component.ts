import { Component } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { CoiDisclosure } from '../../disclosure/coi-interface';
import { TravelDisclosureService } from '../services/travel-disclosure.service';

@Component({
    selector: 'app-travel-questionnaire',
    template: `
        <div id="travel-questionnaire-coi">
            <app-view-questionnaire-list
                    [isShowExportButton]="false"
                    [configuration]="configuration"
                    [questionnaireHeader]="''"
                    [isShowSave]="false"
                    [saveButtonLabel]="'Save and Continue'"
                    (QuestionnaireSaveEvent)="getSaveEvent($event)"
                    (QuestionnaireEditEvent) = "setUnSavedChanges($event)">
            </app-view-questionnaire-list>
        </div>
    `
})
export class TravelQuestionnaireComponent {

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

    constructor(private _commonService: CommonService,
                private _service: TravelDisclosureService) {
                    window.scrollTo(0, 0);
                }

    getSaveEvent(_event: any): void {
        this._service.travelDataChanged = false;
        this._service.unSavedTabName = '';
        if (_event.QUESTIONNAIRE_COMPLETED_FLAG === 'Y') {

        }
    }

    setUnSavedChanges(changeStatus: boolean): void {
        this._service.unSavedTabName = 'Screening Questionnaire';
        this._service.travelDataChanged = changeStatus;
    }

}
