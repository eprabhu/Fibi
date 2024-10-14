import {Component, Input, OnChanges, OnInit} from '@angular/core';
import {
    ENTITY_COMPLIANCE_RIGHT,
    ENTITY_ORGANIZATION_RIGHT,
    ENTITY_SPONSOR_RIGHT, GLOBAL_ENTITY_COMPLIANCE_SUBMODULE_CODE,
    GLOBAL_ENTITY_MODULE_CODE,
    GLOBAL_ENTITY_ORGANIZATION_SUBMODULE_CODE,
    GLOBAL_ENTITY_SPONSOR_SUBMODULE_CODE,
} from '../../../app-constants';
import {CommonService} from '../../../common/services/common.service';
import {EntityDataStoreService} from '../../entity-data-store.service';
import {DataStoreEvent, EntireEntityDetails} from '../entity-interface';
import {deepCloneObject, isEmptyObject} from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import {Subscription} from 'rxjs';
import {
    COMPLIANCE_QUESTIONNAIRE_SECTION_ID, COMPLIANCE_QUESTIONNAIRE_SUB_SECTION_ID,
    SPONSOR_QUESTIONNAIRE_SECTION_ID, SPONSOR_QUESTIONNAIRE_SUB_SECTION_ID,
    SUB_AWARD_QUESTIONNAIRE_SECTION_ID, SUB_AWARD_QUESTIONNAIRE_SUB_SECTION_ID
} from '../entity-constants';

@Component({
  selector: 'app-entity-common-questionnaire',
  templateUrl: './entity-common-questionnaire.component.html',
  styleUrls: ['./entity-common-questionnaire.component.scss']
})
export class EntityCommonQuestionnaireComponent implements OnInit {

    @Input() moduleSubitemCode = 0;
    @Input() sectionName = '';
    @Input() sectionId = '';
    @Input() subSectionId = 0;

    configuration: any = {
        moduleItemCode: GLOBAL_ENTITY_MODULE_CODE,
        moduleSubitemCodes: [],
        moduleItemKey: 0,
        moduleSubItemKey: 0,
        actionUserId: this._commonService.getCurrentUserDetail('personID'),
        actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
        enableViewMode: false,
        isChangeWarning: true,
        isEnableVersion: true,
    };
    entityDetails = null;
    isEditMode = false;
    isQuestionnaireValidateMode = false;
    sectionToRightsMapping = {
        [SPONSOR_QUESTIONNAIRE_SECTION_ID]: {
            right: ENTITY_SPONSOR_RIGHT,
            subModuleCode: GLOBAL_ENTITY_SPONSOR_SUBMODULE_CODE
        },
        [SUB_AWARD_QUESTIONNAIRE_SECTION_ID]: {
            right: ENTITY_ORGANIZATION_RIGHT,
            subModuleCode: GLOBAL_ENTITY_ORGANIZATION_SUBMODULE_CODE
        },
        [COMPLIANCE_QUESTIONNAIRE_SECTION_ID]: {
            right: ENTITY_COMPLIANCE_RIGHT,
            subModuleCode: GLOBAL_ENTITY_COMPLIANCE_SUBMODULE_CODE
        }
    };
    elementNameMapping = {
        [SPONSOR_QUESTIONNAIRE_SUB_SECTION_ID]: 'coi-sponsor-ques-head-',
        [SUB_AWARD_QUESTIONNAIRE_SUB_SECTION_ID]: 'coi-sub-ques-head-',
        [COMPLIANCE_QUESTIONNAIRE_SUB_SECTION_ID]: 'coi-compl-ques-head-',
    };
    $subscriptions: Subscription[] = [];

    constructor(private _commonService: CommonService, private _dataStoreService: EntityDataStoreService) {
    }

    ngOnInit() {
        this.listenDataChangeFromStore();
        this.getDataFromStore();
        this.setQuestionnaireConfiguration();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: DataStoreEvent) => {
                this.getDataFromStore();
                this.updateQuestionnaireEdit();
            })
        );
    }


    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.checkUserHasRight();
    }

    private checkUserHasRight(): void {
        this.isEditMode = this._dataStoreService.getEditMode() && this._commonService.getAvailableRight(this.getTabBasedRights(), 'SOME');
    }

    setQuestionnaireConfiguration() {
        this.configuration.moduleItemKey = this.entityDetails.entityId;
        this.configuration.moduleSubitemCodes = [this.getSubItemCode()];
        this.configuration.enableViewMode = !this.isEditMode;
        this.refreshQuestionnaireConfiguration();
    }

    updateQuestionnaireEdit() {
        if (this.configuration.enableViewMode === this.isEditMode) {
            this.configuration.enableViewMode = !this.isEditMode;
            this.refreshQuestionnaireConfiguration();
        }
    }

    refreshQuestionnaireConfiguration() {
        this.configuration = deepCloneObject(this.configuration);
    }

    getTabBasedRights() {
        return this.sectionToRightsMapping?.[this.sectionId]?.right || [];
    }

    getSubItemCode() {
        return this.sectionToRightsMapping?.[this.sectionId]?.subModuleCode || null;
    }

    getSaveEvent(event) {
        this._commonService.setChangesAvailable(false);
    }

    markQuestionnaireAsEdited(event) {
        this._commonService.setChangesAvailable(true);
    }
}
