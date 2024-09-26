import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { isEmptyObject, openCoiSlider } from 'projects/coi/src/app/common/utilities/custom-utilities';
import { EntireEntityDetails, EntityDetailsInPopup, EntityRisk, EntityRiskCategoryCode, EntityRiskModalDetails, EntityRiskProxyController, EntityRiskRO, RiskLevel, RiskType } from '../../entity-interface';
import { getEntityFullAddress } from '../../../entity-management.service';
import { Subscription } from 'rxjs';
import { EntityRiskSectionService } from '../entity-risk-section.service';
import { CommonService } from 'projects/coi/src/app/common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/coi/src/app/app-constants';
import { EntityDataStoreService } from '../../../entity-data-store.service';
import { deepCloneObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { debounceTime, distinctUntilChanged } from 'rxjs/operators';

@Component({
    selector: 'app-risk-edit-history-slider',
    templateUrl: './risk-edit-history-slider.component.html',
    styleUrls: ['./risk-edit-history-slider.component.scss']
})
export class RiskEditHistorySlider implements OnInit {

    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    @Output() riskUpdated: EventEmitter<EntityRisk[]> = new EventEmitter<EntityRisk[]>();
    @Input() isEditRisk: any;
    @Input() riskCategoryCode: EntityRiskCategoryCode;
    @Input() currentRiskDetails: any;
    @Input() sectionName: any;
    @Input() sectionId: any;
    @Input() subSectionId: any;
    entityCardDetails = new EntityDetailsInPopup();
    entityRiskModalDetails = new EntityRiskModalDetails();
    $subscriptions: Subscription[] = [];
    entityRiskLevelList: RiskLevel[] = [];
    entityRiskLevelOption = 'Empty#Empty#false#false';
    oldRiskLevelCode: any;
    oldRiskLevel: any;
    oldDescription: any;
    mandatoryList = new Map();
    openEditSlider = false;
    isEditMode = true;
    riskHistory = [];
    coiEntityDetails: any = {};
    updatedRiskDetails = new EntityRisk();

    constructor(private _entityRiskSectionService: EntityRiskSectionService, private _commonService: CommonService,
        private _dataStoreService: EntityDataStoreService
    ) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setValuesForHeader();
        this.setDefaultValues();
        this.setRiskTypeCode();
        setTimeout(() => {
            openCoiSlider('risk-edit-history-slider');
        });
    }
    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) {
            return;
        }
        this.coiEntityDetails = ENTITY_DATA.entityDetails;
        this.loadRiskHistory();
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.pipe(debounceTime(200), distinctUntilChanged()).subscribe(() => {
                this.getDataFromStore();
            })
        );
    }

    setValuesForHeader() {
        this.entityCardDetails.entityName = this.coiEntityDetails.entityName;
        this.entityCardDetails.email = this.coiEntityDetails.certifiedEmail;
        this.entityCardDetails.phone = this.coiEntityDetails.phoneNumber;
        this.entityCardDetails.website = this.coiEntityDetails.websiteAddress;
        this.entityCardDetails.entityId = this.coiEntityDetails.entityId;
        this.entityCardDetails.fullAddress = getEntityFullAddress(this.coiEntityDetails);
    }

    setDefaultValues() {
        if (this.currentRiskDetails) {
            this.entityRiskModalDetails.entityRisk = { ...this.currentRiskDetails };
            this.entityRiskModalDetails.entityRisk.description = this.currentRiskDetails.description;
            this.entityRiskModalDetails.entityRisk.riskLevel.description = this.currentRiskDetails.riskLevel.description;
            this.entityRiskModalDetails.entityRisk.riskTypeCode = this.currentRiskDetails.riskTypeCode;
        }
        this.oldRiskLevelCode = this.currentRiskDetails.riskLevelCode;
        this.oldRiskLevel = this.currentRiskDetails.riskLevel.description;
        this.oldDescription = this.currentRiskDetails.description;
    }

    /**
     * This method is used to retrieve the risk level based on the risk type.
     */
    setRiskTypeCode() {
        this.fetchRiskLevels(this.entityRiskModalDetails.entityRisk.riskTypeCode);
    }

    private fetchRiskLevels(riskTypeCode: string): Promise<any> {
        return new Promise((resolve, reject) => {
            if (riskTypeCode) {
                this.$subscriptions.push(
                    this._entityRiskSectionService.fetchRiskLevels(riskTypeCode)
                        .subscribe(
                            (riskLevelList: RiskLevel[]) => {
                                this.entityRiskLevelList = riskLevelList;
                                resolve(this.entityRiskLevelList);
                            },
                            (err) => {
                                this.entityRiskLevelList = [];
                                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                                reject(err);
                            }
                        )
                );
            } else {
                this.entityRiskLevelList = [];
                resolve([]);
            }
        });
    }

    onRiskLevelSelected(event: any[] | null): void {
        this.entityRiskModalDetails.entityRisk.riskLevel = event ? event[0] : null;
        this.entityRiskModalDetails.entityRisk.riskLevelCode = event ? event[0]?.riskLevelCode : null;
        this.entityRiskModalDetails.entityRisk.description = '';
    }


    updateEntityRisk(): void {
        if (this.entityMandatoryValidation()) {
            this.$subscriptions.push(this._entityRiskSectionService.updateEntityRisk(this.getEntityRO(), this.getProxyController()).subscribe((data: any) => {
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Risk updated successfully.');
                this._dataStoreService.enableModificationHistoryTracking();
                const updatedRisk = deepCloneObject(this.entityRiskModalDetails.entityRisk);
                this.updatedRiskDetails = deepCloneObject(updatedRisk);
                this.riskUpdated.emit(updatedRisk);
                setTimeout(() => {
                    this.mandatoryList.clear();
                }, 100);
                this.loadRiskHistory();
            },
            (_err) => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in Updating Risk. Please try again.');
            }));
        }
    }

    private getProxyController(): EntityRiskProxyController {
        switch (this.riskCategoryCode) {
            case 'OR': return '/organization';
            case 'CO': return '/compliance';
            case 'SP': return '/sponsor';
            case 'EN': return '';
            default: return;
        }
    }

    hideConflictNavBar(): void {
        setTimeout(() => {
            this.closePage.emit();
            this.openEditSlider = false;
        }, 500);
    }

    private getEntityRO(): EntityRiskRO {
        const entityRO: any = {
            description: this.entityRiskModalDetails.entityRisk.description,
            entityId: this.entityRiskModalDetails.entityRisk.entityId,
            riskTypeCode: this.entityRiskModalDetails.entityRisk.riskTypeCode,
            entityRiskId: this.entityRiskModalDetails.entityRisk.entityRiskId,
            riskType: this.currentRiskDetails.riskType.description
        };
        if (this.entityRiskModalDetails.entityRisk.riskLevelCode !== this.oldRiskLevelCode) {
            entityRO.riskLevel = this.entityRiskModalDetails.entityRisk.riskLevel.description;
            entityRO.riskLevelCode = this.entityRiskModalDetails.entityRisk.riskLevelCode;
            entityRO.oldRiskLevelCode = this.oldRiskLevelCode;
            entityRO.oldRiskLevel = this.oldRiskLevel;
        }
        if (this.entityRiskModalDetails.entityRisk.description !== this.oldDescription) {
            entityRO.oldDescription = this.oldDescription;
        }

        return entityRO;
    }

    private entityMandatoryValidation(): boolean {
        this.mandatoryList.clear();
        const { riskLevelCode, riskTypeCode, description } = this.entityRiskModalDetails.entityRisk;
        if (!riskTypeCode) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if (!riskLevelCode) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if (!description) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }
        if (((this.entityRiskModalDetails.entityRisk.description === this.currentRiskDetails.description &&
            this.entityRiskModalDetails.entityRisk.riskLevel.description === this.currentRiskDetails.riskLevel.description) ||
            (this.entityRiskModalDetails.entityRisk.description === this.updatedRiskDetails.description &&
                this.entityRiskModalDetails.entityRisk.riskLevel.description === this.updatedRiskDetails.riskLevel.description))) {
            this.mandatoryList.set('duplicateStatus', 'You are trying to update the Risk Description with the current Risk Description of the Entity.');
        }
        return this.mandatoryList.size === 0;
    }

    private loadRiskHistory(): void {
        this.$subscriptions.push(
            this._entityRiskSectionService.loadRiskHistory(this.currentRiskDetails.entityRiskId).subscribe((data: any) => {
                this.riskHistory = data;
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
            }));
    }

    clearRiskSlider(): void {
        setTimeout(() => {
            this.mandatoryList.clear();
            this.entityRiskModalDetails.entityRisk.description = '';
            this.entityRiskModalDetails.entityRisk.riskLevel = null;
            this.entityRiskModalDetails.entityRisk.riskLevelCode = null;
            this.entityRiskModalDetails.selectedRiskLevelLookUpList = [];
        }, 100);
    }
}
