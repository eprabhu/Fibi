import { Component, OnInit, OnDestroy, Input, Output, ViewChild, ElementRef, EventEmitter } from "@angular/core";
import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS, PROJECT_CONFLICT_STATUS_BADGE } from "../../../../app-constants";
import { CommonService } from "../../../../common/services/common.service";
import { openCoiSlider } from "../../../../common/utilities/custom-utilities";
import { deepCloneObject, isEmptyObject } from "../../../../../../../fibi/src/app/common/utilities/custom-utilities";
import { subscriptionHandler } from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import { Subscription } from "rxjs";
import { AddConflictSlider, COI, CoiConflictStatusType, CoiProjConflictStatusType, UpdateProjectRelationshipRO } from "../../../coi-interface";
import { CoiService } from "../../../services/coi.service";
import { DataStoreService } from "../../../services/data-store.service";
import { CoiSummaryEventsAndStoreService } from "../../../summary/services/coi-summary-events-and-store.service";

@Component({
    selector: 'app-add-conflict-slider',
    templateUrl: './add-conflict-slider.component.html',
    styleUrls: ['./add-conflict-slider.component.scss']
})
export class AddConflictSliderComponent implements OnInit, OnDestroy {

    @Input() isEditMode: any = null;
    @Input() addConflictSlider = new AddConflictSlider();
    @Input() coiStatusList: CoiProjConflictStatusType[] = [];
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    @ViewChild('addCommentOverlay', { static: true }) addCommentOverlay: ElementRef;

    coiData = new COI();
    comment: string = '';
    conflictHistory = [];
    personUnitDetails = '';
    isReadMore: boolean[] = [];
    $subscriptions: Subscription[] = [];
    projectConflictStatusCode: string = '';
    projectConflictValidationMap = new Map();
    PROJECT_CONFLICT_STATUS_BADGE = PROJECT_CONFLICT_STATUS_BADGE;

    constructor( public coiService: CoiService,
                private _dataStore: DataStoreService,
                private commonService: CommonService,
                public dataStoreService: CoiSummaryEventsAndStoreService
    ) { }

    ngOnInit(): void {
        this.showConflictNavBar();
        this.getConflictStatusLookup();
        this.loadProjectConflictHistory();
        this.coiData = this._dataStore.getData();
        this.personUnitDetails = this.commonService.getPersonLeadUnitDetails(this.coiData?.coiDisclosure?.person?.unit);
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getConflictStatusLookup(): void {
        if (!this.coiStatusList.length) {
            this.$subscriptions.push(this.dataStoreService.getProjConflictStatusType().subscribe((res: any) => {
                this.coiStatusList = res;
            }));
        }
    }

    private showConflictNavBar(): void {
        if (this.addConflictSlider.isOpenSlider) {
            setTimeout(() => {
                openCoiSlider('add-conflict');
            });
        }
    }

    private loadProjectConflictHistory(): void {
        this.$subscriptions.push(
            this.dataStoreService.loadProjectConflictHistory(this.addConflictSlider.coiDisclEntProjDetail.coiDisclProjectEntityRelId).subscribe((data: any) => {
                this.conflictHistory = data;
                this.isReadMore = [];
            }, _err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
            }));
    }

    private emitUpdatedSfi(): void {
        this.addConflictSlider.coiDisclEntProjDetail.projectConflictStatusCode = this.projectConflictStatusCode;
        const STATUS_TYPE = this.coiStatusList.find((type: any) => type.projectConflictStatusCode === this.projectConflictStatusCode);
        this.addConflictSlider.coiDisclEntProjDetail.coiProjConflictStatusType.description = deepCloneObject(STATUS_TYPE.description);
        this.addConflictSlider.coiDisclEntProjDetail.coiProjConflictStatusType.projectConflictStatusCode = deepCloneObject(STATUS_TYPE.projectConflictStatusCode);
        this.addConflictSlider.coiDisclEntProjDetail.disclComment.comment = this.comment;
        this.commonService.$globalEventNotifier.next({ uniqueId: 'COI_DISCLOSURE_ADD_CONFLICT_UPDATE', content: this.addConflictSlider });
    }

    private updateDisclosureConflictStatus(coiConflictStatusTypeDto: CoiConflictStatusType): void {
        this.coiData.coiDisclosure.coiConflictStatusType = coiConflictStatusTypeDto;
        this.coiData.coiDisclosure.conflictStatusCode = coiConflictStatusTypeDto?.conflictStatusCode;
        this._dataStore.updateStore(['coiDisclosure'], { coiDisclosure: this.coiData.coiDisclosure });
    }

    private getUpdateProjectRelationshipRO(): UpdateProjectRelationshipRO {
        return {
            coiDisclProjectEntityRelId: this.addConflictSlider.coiDisclEntProjDetail.coiDisclProjectEntityRelId,
            documentOwnerPersonId: this.coiData.coiDisclosure.person.personId,
            disclosureId: this.coiData.coiDisclosure.disclosureId,
            conflictStatusCode: this.projectConflictStatusCode,
            comment: this.comment
        };
    }

    private projectConflictValidation(): boolean {
        this.projectConflictValidationMap.clear();
        if (this.projectConflictStatusCode === 'null' || !this.projectConflictStatusCode) {
            this.projectConflictValidationMap.set('coiConflictStatusCode', 'Please select a conflict status.');
        }
        if (!this.comment) {
            this.projectConflictValidationMap.set('comment', 'Please enter the description.');
        }
        if (this.projectConflictStatusCode == this.addConflictSlider.coiDisclEntProjDetail.projectConflictStatusCode) {
            this.projectConflictValidationMap.set('duplicateStatus', 'You are trying to update the conflict with the current conflict status of the disclosure.');
        }
        return this.projectConflictValidationMap.size === 0;
    }

    clearConflictModal(): void {
        this.projectConflictValidationMap.clear();
        this.projectConflictStatusCode = '';
        this.comment = '';
        this.isReadMore = [];
    }

    hideConflictNavBar(): void {
        setTimeout(() => {
            this.closePage.emit();
            this.addConflictSlider.isOpenSlider = false;
        }, 500);
    }

    updateProjectRelationship(): void {
        if (this.projectConflictValidation()) {
            this.$subscriptions.push(
                this.dataStoreService.updateProjectRelationship(this.getUpdateProjectRelationshipRO()).subscribe((data: any) => {
                    this.conflictHistory = data.coiConflictHistoryList;
                    this.emitUpdatedSfi();
                    this.clearConflictModal();
                    this.updateDisclosureConflictStatus(data.coiConflictStatusTypeDto);
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict updated successfully.');
                }, _err => {
                    if (_err.status === 405) {
                        this.coiService.concurrentUpdateAction = 'Modify Conflict';
                    } 
                    else {
                        this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating conflict status. Please try again.');
                    }
                }));
        }
    }

    isEmptyHistory(): boolean {
        return isEmptyObject(this.conflictHistory);
    }

    isFieldValueChanges(): boolean {
        return !!((this.projectConflictStatusCode || this.comment));
    }

}
