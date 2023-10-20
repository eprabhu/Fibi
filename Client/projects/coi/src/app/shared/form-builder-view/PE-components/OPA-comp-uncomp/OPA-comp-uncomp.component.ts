
import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilderService } from '../../form-builder.service';
import { getEndPointForEntity } from '../../search-configurations';
import { CompUnComp, CompUnCompPE, EntitySaveRO, RelationShipSaveRO } from './interface';
import { OPACompUncompService } from './OPA-comp-uncomp.service';
import { parseDateWithoutTimestamp } from 'projects/fibi/src/app/common/utilities/date-utilities';
import { Subject } from 'rxjs';
declare const $: any;

@Component({
    selector: 'app-OPA-comp-uncomp',
    templateUrl: './OPA-comp-uncomp.component.html',
    styleUrls: ['./OPA-comp-uncomp.component.scss'],
    providers: [OPACompUncompService]
})
export class OPACompUncompComponent implements OnInit {

    constructor(private _formBuilder: FormBuilderService, private _api: OPACompUncompService ) { }

    @Input() componentData = new CompUnCompPE();
    @Input() formBuilderId;
    @Input() externalEvents: Subject<any> = new Subject<any>();
    @Output() childEvents: EventEmitter<any> = new EventEmitter<any>();
    id: number;
    entitySearchOptions: any = {};
    entityDetails: any = {};
    compUnCompData = new CompUnComp();
    editIndex = -1;
    deleteIndex: number;
    $subscriptions = [];
    isDuplicate = false;

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
            if (this.compUnCompData.actionType === 'SAVE') {
                this.editIndex === -1 ? this.componentData.data.push(res.data.data[0]) :
                                        this.componentData.data[this.editIndex] = res.data.data[0];
                // $('#OPA_COMP_UNCOMP' + this.id).modal('hide');
            document.getElementById('item_add').click();

            } else if (this.compUnCompData.actionType === 'DELETE' && this.deleteIndex > -1) {
                this.componentData.data.splice(this.deleteIndex, 1);
                // $('#OPA_COMP_UNCOMP_DELETE' + this.id).modal('hide');
            document.getElementById('item_delete').click();
            }
            this.clearData();
        }));
    }

    async addRowItem() {
        const RO: RelationShipSaveRO | EntitySaveRO = this.setEntityROForSave(this.entityDetails);
        try {
            const response = await this._api.saveEntityOrRelation(RO);
            this.setPersonEntityId(response);
            this.setEntityInfoForCompUnComp();
            this.childEvents.emit({action: 'ADD', data: this.compUnCompData});
        } catch (err) {
            if ((err.status === 405)) {
                // this.setPersonEntityId();
                // this.setEntityInfoForCompUnComp();
                // this.childEvents.emit({action: 'ADD', data: this.compUnCompData});
            }
        }
    }

    editEntityItem(compUncomp:  CompUnComp , index): void {
        this.compUnCompData = compUncomp;
        this.editIndex = index;
        this.entityDetails = compUncomp.entityInfo;
    }

    updateEntity() {
        delete this.compUnCompData.updateTimestamp;
        this.compUnCompData.actionType = 'SAVE';
        this.childEvents.emit({action: 'ADD', data: this.compUnCompData});
    }

    deleteEntity() {
        delete this.compUnCompData.updateTimestamp;
        this.compUnCompData.actionType = 'DELETE';
        this.childEvents.emit({action: 'ADD', data: this.compUnCompData});
    }
    entitySelected(entity: any): void {
        if (entity) {
            const index = this.checkDuplicate(entity.personEntityId);
        this.isDuplicate = index === -1 || index === this.editIndex ?  false : true;
        this.entityDetails = entity;
        }
    }


    private setPersonEntityId(response): void {
        if (!this.entityDetails.personEntityId) {
            this.entityDetails.personEntityId = response.personEntityId;
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
            entitySaveRO.staffInvolvement = this.compUnCompData.natureOfWork;
            entitySaveRO.validPersonEntityRelTypeCodes = [5];
            return entitySaveRO;
        }
    }

    private setEntityInfoForCompUnComp(): void {
        this.compUnCompData.entityInfo.countryName = this.entityDetails.countryName;
        this.compUnCompData.entityInfo.entityName = this.entityDetails.entityName;
        this.compUnCompData.entityInfo.entityType = this.entityDetails.entityType;
        this.compUnCompData.entityInfo.relationship = this.entityDetails.validPersonEntityRelType;
        this.compUnCompData.entityInfo.entityRiskCategory = this.entityDetails.entityRiskCategory;
        this.compUnCompData.entityInfo.isRelationshipActive = this.entityDetails.isRelationshipActive ? 'Y' : 'N';
        this.compUnCompData.entityInfo.sfiVersionStatus = this.entityDetails.personEntityVersionStatus;
        this.compUnCompData.entityInfo.involvementStartDate = parseDateWithoutTimestamp(new Date());

        this.compUnCompData.personEntityId = this.entityDetails.personEntityId;
        this.compUnCompData.opaDisclosureId = this.formBuilderId;
        this.compUnCompData.actionType = 'SAVE';
        this.compUnCompData.opaDisclActivityId = null;
    }

    getWarningClass(typeCode): string {
        switch (typeCode) {
            case 'High':
                return 'invalid';
            case 'Medium':
                return 'medium-risk';
            case 'Low':
                return 'low-risk';
            default:
                return;
        }
    }

    viewEntityDetails(id) {
        window.open(window.location.origin + window.location.pathname + '#/coi/entity-management/entity-details?entityManageId=' + id);
    }

    clearData() {
        this.entitySearchOptions =  getEndPointForEntity(this._formBuilder.baseURL);
        this.compUnCompData = new CompUnComp();
        this.entityDetails = {};
        this.editIndex = -1;
        this.deleteIndex = -1;
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

    checkDuplicate(personEntityId) {
       return this.componentData.data.findIndex(E => E.personEntityId === personEntityId);
    }

}
