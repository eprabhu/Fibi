import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Subject } from 'rxjs';
import { FormBuilderService } from '../../form-builder.service';
import { OPACompUncompService } from '../OPA-comp-uncomp/OPA-comp-uncomp.service';
import { getEndPointForEntity } from '../../search-configurations';
import { EntitySaveRO, OutsideFinRelation, OutsideFinRelationPE, RelationShipSaveRO } from './interface';
import { parseDateWithoutTimestamp } from 'projects/fibi/src/app/common/utilities/date-utilities';
import { openInNewTab } from 'projects/coi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-OPA-outside-financial-relation',
    templateUrl: './OPA-outside-financial-relation.component.html',
    styleUrls: ['./OPA-outside-financial-relation.component.scss'],
    providers: [OPACompUncompService]
})
export class OPAOutsideFinancialRelationComponent implements OnInit {

    @Input() componentData = new OutsideFinRelationPE();
    @Input() formBuilderId;
    @Input() externalEvents: Subject<any> = new Subject<any>();
    @Output() childEvents: EventEmitter<any> = new EventEmitter<any>();
    @Input() isFormEditable = true;
    id: number;
    entitySearchOptions: any = {};
    entityDetails: any = {};
    outsideFinRelationData = new OutsideFinRelation();
    editIndex = -1;
    deleteIndex: number;
    $subscriptions = [];
    isDuplicate = false;

    constructor(private _formBuilder: FormBuilderService, private _api: OPACompUncompService) { }


    ngOnInit() {
        this.generateId();
        this.entitySearchOptions = getEndPointForEntity(this._formBuilder.baseURL);
        this.listenForExternalEvents();
    }

    generateId() {
        this.id = new Date().getTime();
    }

    listenForExternalEvents(): void {
        this.$subscriptions.push(this.externalEvents.subscribe(res => {
            if (this.outsideFinRelationData.actionType === 'SAVE') {
                this.editIndex === -1 ? this.componentData.data.push(res.data.data[0]) :
                    this.componentData.data[this.editIndex] = res.data.data[0];
                document.getElementById('OUTSIDE_FIN_REL_ADD-BTN' + this.id).click();

            } else if (this.outsideFinRelationData.actionType === 'DELETE' && this.deleteIndex > -1) {
                this.componentData.data.splice(this.deleteIndex, 1);
                document.getElementById('OUTSIDE_FIN_REL_DELETE-BTN' + this.id).click();
            }
            this.clearData();
        }));
    }

    clearData() {
        this.entitySearchOptions = getEndPointForEntity(this._formBuilder.baseURL);
        this.outsideFinRelationData = new OutsideFinRelation();
        this.entityDetails = {};
        this.editIndex = -1;
        this.deleteIndex = -1;
    }

    async addRowItem() {
        const RO: RelationShipSaveRO | EntitySaveRO = this.setEntityROForSave(this.entityDetails);
        try {
            const response = await this._api.saveEntityOrRelation(RO);
            this.setPersonEntityId(response);
            this.setEntityInfoForOutsideFinRelationData();
            this.childEvents.emit({ action: 'ADD', data: this.outsideFinRelationData });
        } catch (err) {
            if ((err.status === 405)) {
                // this.setPersonEntityId();
                // this.setEntityInfoForCompUnComp();
                // this.childEvents.emit({action: 'ADD', data: this.outsideFinRelationData});
            }
        }
    }

    setEntityROForSave(entity: any): EntitySaveRO | RelationShipSaveRO {
        if (this.entityDetails.personEntityId) {
            const relationRO = new RelationShipSaveRO();
            relationRO.personEntityId = this.entityDetails.personEntityId;
            relationRO.validPersonEntityRelTypeCodes = [5];
            return relationRO;
        } else {
            const entitySaveRO = new EntitySaveRO();
            entitySaveRO.entityId = entity.entityId;
            entitySaveRO.entityNumber = entity.entityNumber;
            entitySaveRO.involvementStartDate = parseDateWithoutTimestamp(new Date());
            entitySaveRO.staffInvolvement = this.outsideFinRelationData.personsRelationWithEntity;
            entitySaveRO.validPersonEntityRelTypeCodes = [5];
            return entitySaveRO;
        }
    }

    private setPersonEntityId(response): void {
        if (!this.entityDetails.personEntityId) {
            this.entityDetails.personEntityId = response.personEntityId;
        }
    }

    private setEntityInfoForOutsideFinRelationData(): void {
        this.outsideFinRelationData.entityInfo.countryName = this.entityDetails.countryName;
        this.outsideFinRelationData.entityInfo.entityName = this.entityDetails.entityName;
        this.outsideFinRelationData.entityInfo.entityType = this.entityDetails.entityType;
        this.outsideFinRelationData.entityInfo.relationship = this.entityDetails.validPersonEntityRelType;
        this.outsideFinRelationData.entityInfo.entityRiskCategory = this.entityDetails.entityRiskCategory;
        this.outsideFinRelationData.entityInfo.isRelationshipActive = this.entityDetails.isRelationshipActive ? 'Y' : 'N';
        this.outsideFinRelationData.entityInfo.sfiVersionStatus = this.entityDetails.personEntityVersionStatus;
        this.outsideFinRelationData.entityInfo.involvementStartDate = parseDateWithoutTimestamp(new Date());

        this.outsideFinRelationData.personEntityId = this.entityDetails.personEntityId;
        this.outsideFinRelationData.opaDisclosureId = this.formBuilderId;
        this.outsideFinRelationData.actionType = 'SAVE';
        this.outsideFinRelationData.opaOutsideFinancialInterestId = null;
    }

    editEntityItem(outsideFinRelation: OutsideFinRelation  , index): void {
        this.outsideFinRelationData = outsideFinRelation;
        this.editIndex = index;
        this.entityDetails = outsideFinRelation.entityInfo;
    }

    updateEntity() {
        delete this.outsideFinRelationData.updateTimestamp;
        this.outsideFinRelationData.actionType = 'SAVE';
        this.childEvents.emit({action: 'UPDATE', data: this.outsideFinRelationData});
    }

    deleteEntity() {
        delete this.outsideFinRelationData.updateTimestamp;
        this.outsideFinRelationData.actionType = 'DELETE';
        this.childEvents.emit({action: 'DELETE', data: this.outsideFinRelationData});
    }

    entitySelected(entity: any): void {
        if (entity) {
            const index = this.checkDuplicate(entity.personEntityId);
        this.isDuplicate = index === -1 || index === this.editIndex ?  false : true;
        this.entityDetails = entity;
        }
    }

    checkDuplicate(personEntityId) {
        return this.componentData.data.findIndex(E => E.personEntityId === personEntityId);
    }

    getClassForStatus(versionStatus, isRelationshipActive) {
        return versionStatus === 'PENDING' ? 'draft-ribbon' :
                  versionStatus === 'ACTIVE' && isRelationshipActive === 'Y' ? 'active-ribbon' :
                  versionStatus === 'ACTIVE' && isRelationshipActive === 'N' ? 'inactive-ribbon' : '';
    }

    getDescriptionForStatus(versionStatus, isRelationshipActive) {
        return versionStatus === 'PENDING' ? 'Draft' :
                  versionStatus === 'ACTIVE' && isRelationshipActive === 'Y' ? 'Active' :
                  versionStatus === 'ACTIVE' && isRelationshipActive === 'N' ? 'inactive' : '';
    }

    viewSlider(personEntityId) {
        openInNewTab('entity-details/entity?', ['personEntityId', 'mode'], [personEntityId, 'view']);
    }

}
