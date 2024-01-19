import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Subject } from 'rxjs';
import { FormBuilderService } from '../../form-builder.service';
import { OPACompUncompService } from '../OPA-comp-uncomp/OPA-comp-uncomp.service';
import { getEndPointForEntity } from '../../search-configurations';
import { EntitySaveRO, OutsideFinRelation, OutsideFinRelationPE, RelationShipSaveRO } from './interface';
import { parseDateWithoutTimestamp } from 'projects/fibi/src/app/common/utilities/date-utilities';
import { openInNewTab } from 'projects/coi/src/app/common/utilities/custom-utilities';
import { trigger, animate, keyframes, transition, style, query, stagger} from '@angular/animations';
import { deepCloneObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';


export const leftSlideInOut = trigger('leftSlideInOut', [
    transition(':enter', [
      animate('300ms ease-in-out', keyframes([
        style({ opacity: 0, transform: 'translateX(-20px)', offset: 0 }),
        style({ opacity: .3, transform: 'translateX(-10px)', offset: 0.3 }),
        style({ opacity: 1, transform: 'translateX(0)', offset: 1.0 }),
      ]))
    ]),
    transition(':leave', [
      animate('300ms ease-in-out', keyframes([
        style({ opacity: 1, transform: 'translateX(0)', offset: 0 }),
        style({ opacity: .3, transform: 'translateX(-5px)', offset: 0.3 }),
        style({ opacity: 0, transform: 'translateX(-10px)', offset: 1.0 }),
      ]))
    ])
  ]);
  export const listAnimation = trigger('listAnimation', [
    transition('* => *', [
      query(':enter', [
        style({ opacity: 0, transform: 'translateY(-10px)' }),
        stagger('100ms', [
          animate('400ms cubic-bezier(0.35, 0, 0.25, 1)',
            style({ opacity: 1, transform: 'translateY(0)' }))
        ])], { optional: true }
      )
    ])
  ]);


@Component({
    selector: 'app-OPA-outside-financial-relation',
    templateUrl: './OPA-outside-financial-relation.component.html',
    styleUrls: ['./OPA-outside-financial-relation.component.scss'],
    providers: [OPACompUncompService],
    animations: [leftSlideInOut, listAnimation]
})
export class OPAOutsideFinancialRelationComponent implements OnInit {

    @Input() componentData = new OutsideFinRelationPE();
    @Input() formBuilderId;
    @Input() externalEvents: Subject<any> = new Subject<any>();
    @Output() childEvents: EventEmitter<any> = new EventEmitter<any>();
    @Input() isFormEditable = true;
    @Input() sectionHeading = '';
    id: number;
    entitySearchOptions: any = {};
    entityDetails: any = {};
    outsideFinRelationData = new OutsideFinRelation();
    editIndex = -1;
    deleteIndex: number;
    $subscriptions = [];
    isDuplicate = false;
    myEntities = [];
    filteredEntities = [];
    currentTab: 'MY_ENTITIES'| 'ADD_ENTITY' = 'MY_ENTITIES';
    currentFilter: 'ALL' | 'INCOMPLETE' | 'COMPLETE' | 'INACTIVE' = 'ALL';
    eventType: 'LINK'| 'NEW' =  'NEW';
    relationshipTypeCache = {};

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
                if (this.eventType === 'NEW') {
                    document.getElementById('OUTSIDE_FIN_REL_ADD_BTN' + this.id).click();
                }
                if (this.eventType === 'LINK') {
                    this.removeFromMyEntities();
                }

            } else if (this.outsideFinRelationData.actionType === 'DELETE' && this.deleteIndex > -1) {
                this.componentData.data.splice(this.deleteIndex, 1);
                document.getElementById('OUTSIDE_FIN_REL_DELETE_BTN' + this.id).click();
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
        this.currentTab = 'MY_ENTITIES';
        this.isDuplicate = false;
    }

    async addRowItem() {
        if (this.isDuplicate) {
            return null;
        }
        const RO: RelationShipSaveRO | EntitySaveRO = this.setEntityROForSave(this.entityDetails);
        try {
            const response = await this._api.saveEntityOrRelation(RO);
            this.setPersonEntityId(response);
            this.setEntityInfoForOutsideFinRelationData();
            this.childEvents.emit({ action: 'ADD', data: this.outsideFinRelationData });
        } catch (err) {
            if ((err.status === 405)) {
                this.setEntityInfoForOutsideFinRelationData();
                this.childEvents.emit({action: 'ADD', data: this.outsideFinRelationData});
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
        this.outsideFinRelationData.entityInfo.isFormCompleted = this.entityDetails.isFormCompleted ? 'Y' : 'N';
        this.outsideFinRelationData.entityInfo.sfiVersionStatus = this.entityDetails.personEntityVersionStatus;
        this.outsideFinRelationData.entityInfo.involvementStartDate = parseDateWithoutTimestamp(new Date());

        this.outsideFinRelationData.personEntityId = this.entityDetails.personEntityId;
        this.outsideFinRelationData.opaDisclosureId = this.formBuilderId;
        this.outsideFinRelationData.actionType = 'SAVE';
        this.outsideFinRelationData.opaOutsideFinancialInterestId = null;
    }

    editEntityItem(outsideFinRelation: OutsideFinRelation  , index): void {
        this.currentTab = 'ADD_ENTITY';
        this.outsideFinRelationData = deepCloneObject(outsideFinRelation);
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

    getClassForStatus(versionStatus, isFormCompleted) {
        return versionStatus === 'ACTIVE' || versionStatus == 'ARCHIVE' ? (isFormCompleted == 'Y' || isFormCompleted === true) ? 't-active-ribbon' : 't-incomplete-ribbon' : 't-inactive-ribbon';
    }

    getClassForStatusInModal(versionStatus, isFormCompleted) {
        return versionStatus === 'ACTIVE' || versionStatus == 'ARCHIVE' ? (isFormCompleted == 'Y' || isFormCompleted === true) ? 'active-ribbon' : 'incomplete-ribbon' : 'inactive-ribbon';
    }

    getDescriptionForStatus(versionStatus, isFormCompleted) { 
        return versionStatus === 'ACTIVE' || versionStatus == 'ARCHIVE' ? (isFormCompleted == 'Y' || isFormCompleted === true) ? 'Complete' : 'Incomplete' : 'Inactive';
    }

    getClassForStatusInModalCard(versionStatus, isFormCompleted) {
        return versionStatus === 'ACTIVE' || versionStatus == 'ARCHIVE' ? (isFormCompleted == 'Y' || isFormCompleted === true) ? 'status-complete' : 'status-incomplete' : 'status-inactive';
    }

    viewSlider(personEntityId) {
        openInNewTab('entity-details/entity?', ['personEntityId', 'mode'], [personEntityId, 'view']);
    }

    getMyEntities(): void {
        this._api.getEntities().subscribe((data: any) => {
            this.myEntities = data;
            this.markLinkableEntities();
            this.setFilter(this.currentFilter);
        });
    }

    markLinkableEntities() {
        this.myEntities = this.myEntities.filter(E => !!!this.componentData.data.find(P => P.personEntityId === E.personEntityId));
    }

    linkEntity(entity) {
        this.eventType = 'LINK';
        this.entityDetails = entity;
        this.addRowItem();
    }

    setFilter(filterType: 'ALL' | 'INCOMPLETE' | 'COMPLETE' | 'INACTIVE') {
        this.currentFilter = filterType;
        switch (this.currentFilter) {
            case 'ALL' : this.filteredEntities = this.myEntities; break;
            case 'COMPLETE' : this.filteredEntities =
                this.myEntities.filter(E => (E.personEntityVersionStatus === 'ACTIVE' || E.personEntityVersionStatus === 'ARCHIVE') && E.isFormCompleted); break;
            case 'INACTIVE' : this.filteredEntities =
                this.myEntities.filter(E => E.personEntityVersionStatus === 'INACTIVE'); break;
            case 'INCOMPLETE' : this.filteredEntities =
                this.myEntities.filter(E => (E.personEntityVersionStatus === 'ACTIVE' || E.personEntityVersionStatus === 'ARCHIVE') && !E.isFormCompleted); break;
        }
    }

    removeFromMyEntities() {
        let INDEX = this.filteredEntities.findIndex(E => this.entityDetails.personEntityId === E.personEntityId);
        if (INDEX > -1) {
            this.filteredEntities.splice(INDEX, 1);
        }
        INDEX = this.myEntities.findIndex(E => this.entityDetails.personEntityId === E.personEntityId);
        if (INDEX > -1) {
            this.myEntities.splice(INDEX, 1);
        }
    }

    getEntityRelationTypePills(validPersonEntityRelType: string) {
        if (this.relationshipTypeCache[validPersonEntityRelType]) {
            return this.relationshipTypeCache[validPersonEntityRelType];
        }
        const entityRelTypes = validPersonEntityRelType.split(':;:');
        this.relationshipTypeCache[validPersonEntityRelType] = entityRelTypes.map(entity => {
            const relationshipType = entity.split(':');
            return {relationshipType: relationshipType[0] || '', description: relationshipType[1] || ''};
        });
        return this.relationshipTypeCache[validPersonEntityRelType];
    }

}
