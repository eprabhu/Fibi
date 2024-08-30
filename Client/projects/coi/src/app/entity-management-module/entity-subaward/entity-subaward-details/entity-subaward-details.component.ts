import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { DATE_PLACEHOLDER } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntityManagementService } from '../../entity-management.service';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { EntireEntityDetails, EntityDetails, showEntityToast } from '../../shared/entity-interface';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntitySubAwardService } from '../entity-subaward.service';
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

    constructor(public commonService: CommonService, private _dataStoreService: EntityDataStoreService, private _autoSaveService: AutoSaveService, private _entityManagementService: EntityManagementService, private _entitySubAwardService: EntitySubAwardService) { }

    ngOnInit() {
        this.triggerSingleSave();
        this.autoSaveSubscribe();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions)
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        if (this._entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.organizationTypeCode) {
            this.selectedLookupList.push({ code: this._entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO.organizationTypeCode, description: null });
            this.samExpirationDate = getDateObjectFromTimeStamp(this._entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.samExpirationDate);
            this.subAwdRiskAssmtDate = getDateObjectFromTimeStamp(this._entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.subAwdRiskAssmtDate);
        }
        this.isEditMode = this._dataStoreService.getEditMode();
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
            if (this._entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.id) {
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
            this._entitySubAwardService.organizationDetailsAutoSave(this.autoSaveRO)
                .subscribe((data: any) => {
                    this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO = {
                        entityId: this.entityDetails.entityId,
                        id: data?.id,
                    }
                    this.autoSaveRO = {};
                    this.isRestrictSave = false;
                    this._entityManagementService.hasChangesAvailable = false;
                    this.dataChangeCounter--;
                    showEntityToast('SUCCESS');
                }, (_error: any) => {
                    this.isRestrictSave = false;
                    showEntityToast('ERROR');
                }));
    }

    private updateSubAwardOrganizationDetails(): void {
        this.autoSaveRO.entityId = this.entityDetails.entityId;
        this.$subscriptions.push(
            this._entitySubAwardService.updateOrganizationDetails(this.autoSaveRO)
                .subscribe((data: any) => {
                    this.autoSaveRO = {};
                    this._entityManagementService.hasChangesAvailable = false;
                    this.dataChangeCounter--;
                    showEntityToast('SUCCESS');
                }, (_error: any) => {
                    showEntityToast('ERROR');
                }));
    }

    onDateSelect(dateType: 'SAM_EXPIRATION' | 'RISK_ASSESSMENT'): void {
        if (dateType == 'SAM_EXPIRATION') {
            this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.samExpirationDate = parseDateWithoutTimestamp(this.samExpirationDate);
            this.changeEvent('samExpirationDate');
        }
        if (dateType == 'RISK_ASSESSMENT') {
            this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.subAwdRiskAssmtDate = parseDateWithoutTimestamp(this.subAwdRiskAssmtDate)
            this.changeEvent('subAwdRiskAssmtDate');
        }
    }

    changeEvent(key: string): void {
        this._entityManagementService.hasChangesAvailable = true;
        if (this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO[key]) {
            this.autoSaveRO[key] = this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO[key];
            this.$debounceEvent.next(key);
        }
    }

    onOrganizationTypeSelect(event: any): void {
        this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.organizationTypeCode = event ? event[0]?.code : null;
        this.changeEvent('organizationTypeCode');
    }
}
