import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilderService } from '../../form-builder.service';
import { openInNewTab } from 'projects/coi/src/app/common/utilities/custom-utilities';
import { parseDateWithoutTimestamp } from 'projects/fibi/src/app/common/utilities/date-utilities';
import { getEndPointForEntity } from '../../search-configurations';
import { Subject } from 'rxjs';
import { OPAInstituteResourcesService } from './OPA-institute-resources.service';
import { leftSlideInOut, listAnimation } from '../OPA-comp-uncomp/OPA-comp-uncomp.component';
import { deepCloneObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntitySaveRO, OPAInstituteResources, OPAInstituteResourcesPE, RelationShipSaveRO } from './OPA-institute-resources.interface';

@Component({
    selector: 'app-OPA-institute-resources',
    templateUrl: './OPA-institute-resources.component.html',
    styleUrls: ['./OPA-institute-resources.component.scss'],
    providers: [OPAInstituteResourcesService],
    animations: [listAnimation, leftSlideInOut]
})
export class OPAInstituteResourceUseComponent implements OnInit {

    @Input() componentData = new OPAInstituteResourcesPE();
    @Input() isFormEditable = true;
    @Input() formBuilderId;
    @Input() externalEvents: Subject<any> = new Subject<any>();
    @Output() childEvents: EventEmitter<any> = new EventEmitter<any>();
    currentFilter: 'ALL' | 'ACTIVE' | 'DRAFT' | 'INACTIVE' = 'ALL';
    currentTab: 'MY_ENTITIES' | 'ADD_ENTITY' = 'MY_ENTITIES';
    eventType: 'LINK' | 'NEW' = 'NEW';
    myEntities = [];
    filteredEntities = [];
    $subscriptions = [];
    relationshipTypeCache = {};
    entityDetails: any = {};
    editIndex = -1;
    id: number;
    deleteIndex: number;
    entitySearchOptions: any = {};
    useOfInstituteResourcesData = new OPAInstituteResources();
    isDuplicate = false;

    constructor( private _formBuilder: FormBuilderService, private _api: OPAInstituteResourcesService ) { }

    ngOnInit(): void {
        this.generateId();
        this.entitySearchOptions = getEndPointForEntity(this._formBuilder.baseURL);
        this.listenForExternalEvents();
    }

    getMyEntities(): void {
        this._api.getEntities().subscribe((data: any) => {
            this.myEntities = data;
            this.markLinkableEntities();
            this.setFilter(this.currentFilter);
        });
    }

    markLinkableEntities(): void {
        this.myEntities = this.myEntities.filter(E => !this.componentData.data.find(P => P.personEntityId === E.personEntityId));
    }

    setFilter(filterType: 'ALL' | 'ACTIVE' | 'DRAFT' | 'INACTIVE'): void {
        this.currentFilter = filterType;
        switch (this.currentFilter) {
            case 'ALL': this.filteredEntities = this.myEntities; break;
            case 'ACTIVE': this.filteredEntities =
                this.myEntities.filter(E => E.personEntityVersionStatus === 'ACTIVE' && E.isRelationshipActive); break;
            case 'INACTIVE': this.filteredEntities =
                this.myEntities.filter(E => E.personEntityVersionStatus === 'ACTIVE' && !E.isRelationshipActive); break;
            case 'DRAFT': this.filteredEntities =
                this.myEntities.filter(E => E.personEntityVersionStatus === 'PENDING'); break;
        }
    }

    getClassForStatus(versionStatus: string, isRelationshipActive: string): string {
        if (typeof (isRelationshipActive) === 'boolean') {
            isRelationshipActive = isRelationshipActive === true ? 'Y' : 'N';
        }
        return versionStatus === 'PENDING' ? 't-draft-ribbon' :
            versionStatus === 'ACTIVE' && isRelationshipActive === 'Y' ? 't-active-ribbon' :
                versionStatus === 'ACTIVE' && isRelationshipActive === 'N' ? 't-inactive-ribbon' : '';
    }

    getDescriptionForStatus(versionStatus: string, isRelationshipActive: string): string {
        if (typeof (isRelationshipActive) === 'boolean') {
            isRelationshipActive = isRelationshipActive === true ? 'Y' : 'N';
        }
        return versionStatus === 'PENDING' ? 'Incomplete' :
            versionStatus === 'ACTIVE' && isRelationshipActive === 'Y' ? 'Active' :
                versionStatus === 'ACTIVE' && isRelationshipActive === 'N' ? 'Inactive' : '';
    }

    viewSlider(personEntityId): void {
        openInNewTab('entity-details/entity?', ['personEntityId', 'mode'], [personEntityId, 'view']);
    }

    getEntityRelationTypePills(validPersonEntityRelType: string) {
        if (this.relationshipTypeCache[validPersonEntityRelType]) {
            return this.relationshipTypeCache[validPersonEntityRelType];
        }
        const entityRelTypes = validPersonEntityRelType.split(':;:');
        this.relationshipTypeCache[validPersonEntityRelType] = entityRelTypes.map(entity => {
            const relationshipType = entity.split(':');
            return { relationshipType: relationshipType[0] || '', description: relationshipType[1] || '' };
        });
        return this.relationshipTypeCache[validPersonEntityRelType];
    }

    editEntityItem(outsideFinRelationData: any , index): void {
        this.useOfInstituteResourcesData = deepCloneObject(outsideFinRelationData);
        this.editIndex = index;
        this.entityDetails = outsideFinRelationData.entityInfo;
        this.currentTab = 'ADD_ENTITY';
    }

    updateEntity(): void {
        delete this.useOfInstituteResourcesData.updateTimestamp;
        this.useOfInstituteResourcesData.actionType = 'SAVE';
        this.childEvents.emit({action: 'UPDATE', data: this.useOfInstituteResourcesData});
    }

    generateId(): void {
        this.id = new Date().getTime();
    }

    viewEntityDetails(id): void {
        window.open(window.location.origin + window.location.pathname + '#/coi/entity-management/entity-details?entityManageId=' + id);
    }

    getClassForStatusInModal(versionStatus: string, isRelationshipActive: string): string {
        if (typeof (isRelationshipActive) === 'boolean') {
            isRelationshipActive = isRelationshipActive === true ? 'Y' : 'N';
        }
        return versionStatus === 'PENDING' ? 'draft-ribbon' :
            versionStatus === 'ACTIVE' && isRelationshipActive === 'Y' ? 'active-ribbon' :
                versionStatus === 'ACTIVE' && isRelationshipActive === 'N' ? 'inactive-ribbon' : '';
    }

    linkEntity(entity): void {
        this.eventType = 'LINK';
        this.entityDetails = entity;
        this.addRowItem();
    }

    async addRowItem() {
        if (this.isDuplicate) {
            return null;
        }
        const RO: RelationShipSaveRO | EntitySaveRO = this.setEntityROForSave(this.entityDetails);
        try {
            const response = await this._api.saveEntityOrRelation(RO);
            this.setPersonEntityId(response);
            this.setEntityInfoForInstResources();
            this.childEvents.emit({ action: 'ADD', data: this.useOfInstituteResourcesData });
        } catch (err) {
            if ((err.status === 405)) {
                this.setEntityInfoForInstResources();
                this.childEvents.emit({ action: 'ADD', data: this.useOfInstituteResourcesData });
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
            entitySaveRO.validPersonEntityRelTypeCodes = [5];
            return entitySaveRO;
        }
    }

    private setPersonEntityId(response): void {
        if (!this.entityDetails.personEntityId) {
            this.entityDetails.personEntityId = response.personEntityId;
        }
    }

    private setEntityInfoForInstResources(): void {
        this.useOfInstituteResourcesData.personEntityId = this.entityDetails.personEntityId;
        this.useOfInstituteResourcesData.opaDisclosureId = this.formBuilderId;
        this.useOfInstituteResourcesData.actionType = 'SAVE';
    }
    
    deleteEntity(): void {
        delete this.useOfInstituteResourcesData.updateTimestamp;
        this.useOfInstituteResourcesData.actionType = 'DELETE';
        this.childEvents.emit({ action: 'DELETE', data: this.useOfInstituteResourcesData });
    }

    clearData() {
        this.entitySearchOptions = getEndPointForEntity(this._formBuilder.baseURL);
        this.entityDetails = {};
        this.editIndex = -1;
        this.deleteIndex = -1;
        this.currentTab = 'MY_ENTITIES';
        this.useOfInstituteResourcesData = new OPAInstituteResources();
        this.isDuplicate = false;
    }

    listenForExternalEvents(): void {
        this.$subscriptions.push(this.externalEvents.subscribe(res => {
            if (this.useOfInstituteResourcesData.actionType === 'SAVE') {
                this.editIndex === -1 ? this.componentData.data.push(res.data.data[0]) :
                    this.componentData.data[this.editIndex] = res.data.data[0];
                if (this.eventType === 'NEW') {
                    document.getElementById('OPA_INST_RES_ADD_BTN' + this.id).click();
                }
                if (this.eventType === 'LINK') {
                    this.removeFromMyEntities();
                }
            } else if (this.useOfInstituteResourcesData.actionType === 'DELETE' && this.deleteIndex > -1) {
                this.componentData.data.splice(this.deleteIndex, 1);
                document.getElementById('OPA_INST_RES_DELETE_BTN' + this.id).click();
            }
            this.clearData();
        }));
    }

    removeFromMyEntities(): void {
        let INDEX = this.filteredEntities.findIndex(E => this.entityDetails.personEntityId === E.personEntityId);
        if (INDEX > -1) {
            this.filteredEntities.splice(INDEX, 1);
        }
        INDEX = this.myEntities.findIndex(E => this.entityDetails.personEntityId === E.personEntityId);
        if (INDEX > -1) {
            this.myEntities.splice(INDEX, 1);
        }
    }

    entitySelected(entity: any): void {
        if (entity) {
            const index = this.checkDuplicate(entity.personEntityId);
            this.isDuplicate = index === -1 || index === this.editIndex ?  false : true;
            this.entityDetails = entity;
        }
    }

    checkDuplicate(personEntityId): number {
        return this.componentData.data.findIndex(E => E.personEntityId === personEntityId);
    }

}
