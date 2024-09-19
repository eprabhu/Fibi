import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { DATE_PLACEHOLDER } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntityManagementService } from '../../entity-management.service';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { EntireEntityDetails, EntityDetails, EntityTabStatus, showEntityToast } from '../../shared/entity-interface';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntitySubAwardService, isOrganizationConditionSatisfied } from '../entity-subaward.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-subaward-details',
    templateUrl: './entity-subaward-details.component.html',
    styleUrls: ['./entity-subaward-details.component.scss']
})
export class EntitySubawardDetailsComponent implements OnInit, OnDestroy {

    @Input() sectionName: any;
    @Input() sectionId: any;
    ORGANIZATION_TYPE_OPTIONS = 'entity_organization_type#ORGANIZATION_TYPE_CODE#false#false';
    DATE_PLACEHOLDER = DATE_PLACEHOLDER;
    samExpirationDate: any;
    subAwdRiskAssmtDate: any;
    dataChangeCounter = 0;
    $debounceEvent = new Subject<any>();
    $subscriptions: Subscription[] = [];
    autoSaveRO: any = {};
    entityDetails: EntityDetails;
    selectedLookupList: any[] = [];
    isRestrictSave = false;
    isEditMode = false;
    entityTabStatus: EntityTabStatus = new EntityTabStatus();

    constructor(public commonService: CommonService,
                private _dataStoreService: EntityDataStoreService,
                private _autoSaveService: AutoSaveService,
                private _entityManagementService: EntityManagementService,
                public entitySubAwardService: EntitySubAwardService
    ) { }

    ngOnInit() {
        this.triggerSingleSave();
        this.autoSaveSubscribe();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.checkUserHasRight();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        if (this.entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.entityOrganizationType?.organizationTypeCode) {
            this.selectedLookupList.push({
                code: this.entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO.entityOrganizationType?.organizationTypeCode,
                description: null
            });
        }
        this.samExpirationDate = getDateObjectFromTimeStamp(this.entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.samExpirationDate);
        this.subAwdRiskAssmtDate = getDateObjectFromTimeStamp(this.entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.subAwdRiskAssmtDate);
        this.isEditMode = this._dataStoreService.getEditMode();
        this.entityTabStatus = ENTITY_DATA?.entityTabStatus;
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    triggerSingleSave(): void {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(2000))).subscribe((data: any) => {
            if (data) {
                this.dataChangeCounter++;
                this._autoSaveService.commonSaveTrigger$.next(true);
            }
        }
        ));
    }

    autoSaveSubscribe(): void {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe((event: any) => {
            this.autoSaveAPI();
        }));
    }

    autoSaveAPI(): void {
        if (this.dataChangeCounter > 0 && !this.isRestrictSave) {
            this.commonService.setLoaderRestriction();
            if (this.entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.id) {
                this.updateSubAwardOrganizationDetails();
            } else {
                this.isRestrictSave = true;
                this.saveSubAwardOrganizationDetails();
            }
            this.commonService.removeLoaderRestriction();
        }
    }

    private saveSubAwardOrganizationDetails(): void {
        this.autoSaveRO.entityId = this.entityDetails.entityId;
        this.$subscriptions.push(
            this.entitySubAwardService.organizationDetailsAutoSave(this.autoSaveRO)
                .subscribe((data: any) => {
                    this.updateHeaderStatus();
                    this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.entityId =
                        this.entityDetails.entityId;
                    this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.id = data?.id;
                    this.autoSaveRO = {};
                    this.isRestrictSave = false;
                    this.dataChangeCounter--;
                    showEntityToast('SUCCESS');
                    this.commonService.setChangesAvailable(false);
                }, (_error: any) => {
                    this.isRestrictSave = false;
                    showEntityToast('ERROR');
                }));
    }

    private updateSubAwardOrganizationDetails(): void {
        this.autoSaveRO.entityId = this.entityDetails.entityId;
        this.$subscriptions.push(
            this.entitySubAwardService.updateOrganizationDetails(this.autoSaveRO)
                .subscribe((data: any) => {
                    this.updateHeaderStatus();
                    this.autoSaveRO = {};
                    this.dataChangeCounter--;
                    showEntityToast('SUCCESS');
                    this.commonService.setChangesAvailable(false);
                }, (_error: any) => {
                    showEntityToast('ERROR');
                }));
    }

    onDateSelect(dateType: 'SAM_EXPIRATION' | 'RISK_ASSESSMENT'): void {
        this.commonService.setChangesAvailable(true);
        if (dateType == 'SAM_EXPIRATION') {
            this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.samExpirationDate =
                parseDateWithoutTimestamp(this.samExpirationDate);
            this.changeEvent('samExpirationDate');
        }
        if (dateType == 'RISK_ASSESSMENT') {
            this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.subAwdRiskAssmtDate =
                parseDateWithoutTimestamp(this.subAwdRiskAssmtDate);
            this.changeEvent('subAwdRiskAssmtDate');
        }
    }

    changeEvent(key: string): void {
       this.commonService.setChangesAvailable(true);
        const IS_ORG_TYPE_CODE = key === 'organizationTypeCode';
        const serviceDTO = this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO;
        if (serviceDTO[key] || IS_ORG_TYPE_CODE) {
            this.autoSaveRO[key] = IS_ORG_TYPE_CODE ? serviceDTO.entityOrganizationType.organizationTypeCode : serviceDTO[key];
            this.$debounceEvent.next(key);
        }
    }

    onOrganizationTypeSelect(event: any): void {
        const { code, description } = event ? event[0] : { code: null, description: null };
        if (!this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.entityOrganizationType) {
            this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.entityOrganizationType =
                new EntityOrganizationType();
        }
        const entityOrganizationType =
            this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.entityOrganizationType;
        entityOrganizationType.organizationTypeCode = code;
        entityOrganizationType.description = description;
        this.changeEvent('organizationTypeCode');
    }

    updateHeaderStatus() {
       if(isOrganizationConditionSatisfied(this.entitySubAwardService.entitySubAwardOrganization)) {
            this.entityTabStatus.entity_sub_org_info = true;
            this._dataStoreService.updateStore(['entityTabStatus'], { 'entityTabStatus':  this.entityTabStatus });
       }
    }

    checkUserHasRight(): void {
        const hasRight = this.commonService.getAvailableRight(['MANAGE_ENTITY_ORGANIZATION'], 'SOME');
        if (!hasRight) {
            this.isEditMode = false;
        }
    }

}
