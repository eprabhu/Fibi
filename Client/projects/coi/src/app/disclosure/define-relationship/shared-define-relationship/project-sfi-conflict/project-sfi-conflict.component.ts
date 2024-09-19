import { Component, OnInit, Input, ChangeDetectionStrategy, OnDestroy, ChangeDetectorRef } from '@angular/core';
import { CommonService } from '../../../../common/services/common.service';
import { DataStoreService } from '../../../services/data-store.service';
import { focusElementById, openCommonModal } from '../../../../common/utilities/custom-utilities';
import { deepCloneObject, isEmptyObject } from '../../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { DefineRelationshipService } from '../../services/define-relationship.service';
import { COI, CoiDisclEntProjDetail, CoiProjConflictStatusType, DefineRelationshipDataStore, ProjectSfiRelationConflictRO, ProjectSfiRelations, SaveProjectSfiConflict } from '../../../coi-interface';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { COMMON_ERROR_TOAST_MSG, HTTP_ERROR_STATUS, PROJECT_CONFLICT_STATUS_BADGE } from '../../../../app-constants';
import { DefineRelationshipDataStoreService } from '../../services/define-relationship-data-store.service';
import { GlobalEventNotifier } from '../../../../common/services/coi-common.interface';
import { CoiService } from '../../../services/coi.service';

@Component({
    selector: 'app-project-sfi-conflict',
    templateUrl: './project-sfi-conflict.component.html',
    styleUrls: ['./project-sfi-conflict.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class ProjectSfiConflictComponent implements OnInit, OnDestroy {

    @Input() projectSfiRelation = new ProjectSfiRelations();

    coiData = new COI();
    $subscriptions: Subscription[] = [];
    mandatoryList: Map<string, string> = new Map();
    PROJECT_CONFLICT_STATUS_BADGE = PROJECT_CONFLICT_STATUS_BADGE;
    isDesc: { [key: string]: boolean } = {};
    currentSortStateKey: string | null = null;
    helpText = `Click 'Apply to All' to update the Conflict of Interest (COI) status and description simultaneously for all Project SFI relationships.`;

    constructor(private _commonService: CommonService,
        private _coiService: CoiService,
        public defineRelationshipService: DefineRelationshipService,
        private _dataStore: DataStoreService, private _cdr: ChangeDetectorRef,
        private _defineRelationshipDataStore: DefineRelationshipDataStoreService) { }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.listenAddConflictSliderChange();
        this.listenDataChangeFromRelationStore();
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore(): void {
        const COI_DATA = this._dataStore.getData();
        if (isEmptyObject(COI_DATA)) { return; }
        this.coiData = COI_DATA;
    }

    private listenDataChangeFromRelationStore(): void {
        this.$subscriptions.push(
            this._defineRelationshipDataStore.$relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.projectId == 'All' || changes.projectId == this.projectSfiRelation.projectId) {
                    this._cdr.markForCheck();
                }
            })
        );
    }

    private listenAddConflictSliderChange(): void {
        this.$subscriptions.push(
            this._commonService.$globalEventNotifier.subscribe((event: GlobalEventNotifier) => {
                if (event.uniqueId === 'COI_DISCLOSURE_ADD_CONFLICT_UPDATE' && event.content.projectSfiRelations.projectId === this.projectSfiRelation.projectId) {
                    this.updateSfiDetails(event.content.coiDisclEntProjDetail);
                }
            })
        );
    }

    private validateProjectSfiConflict(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): boolean {
        const IS_VALID_CONFLICT_STATUS = this.validateConflictStatus(sfiDetails, sfiIndex);
        const IS_VALID_CONFLICT_COMMENT = this.validateDescription(sfiDetails, sfiIndex);
        this.focusElementIfError(IS_VALID_CONFLICT_STATUS, IS_VALID_CONFLICT_COMMENT, sfiIndex);
        return IS_VALID_CONFLICT_STATUS && IS_VALID_CONFLICT_COMMENT;
    }

    private validateConflictStatus(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): boolean {
        this.mandatoryList.delete('CONFLICT_STATUS_' + sfiIndex);
        if (!sfiDetails?.projectConflictStatusCode) {
            this.mandatoryList.set('CONFLICT_STATUS_' + sfiIndex, 'Please select a conflict status.');
        }
        return !this.mandatoryList.has('CONFLICT_STATUS_' + sfiIndex);
    }

    private focusElementIfError(isValidConflictStatus: boolean, isValidConflictComment: boolean, sfiIndex: number): void {
        if (!isValidConflictStatus) {
            focusElementById('CONFLICT_STATUS_' + sfiIndex);
        } else if (!isValidConflictComment) {
            focusElementById('CONFLICT_COMMENT_' + sfiIndex);
        }
    }

    openApplyToAllModal(): void {
        this.defineRelationshipService.applyToAllModal.isOpenModal = true;
        this.defineRelationshipService.applyToAllModal.coiDisclProjectId = this.projectSfiRelation.coiDisclProjectId;
        setTimeout(() => {
            openCommonModal('coi-relation-modal');
        });
    }

    validateDescription(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): boolean {
        this.mandatoryList.delete('CONFLICT_COMMENT_' + sfiIndex);
        if (!sfiDetails?.disclComment?.comment) {
            this.mandatoryList.set('CONFLICT_COMMENT_' + sfiIndex, 'Please enter the description.');
        }
        return !this.mandatoryList.has('CONFLICT_COMMENT_' + sfiIndex);
    }

    saveProjectSfiConflict(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): void {
        if (this.validateProjectSfiConflict(sfiDetails, sfiIndex) && sfiDetails?.isDataChanged) {
            // this._commonService.setLoaderRestriction();
            this.$subscriptions.push(
                this.defineRelationshipService.saveProjectSfiConflict(this.getProjectSfiRelationConflictRO(sfiDetails))
                    .subscribe((res: SaveProjectSfiConflict) => {
                        this.updateSfiDetails(sfiDetails, res.conflictDetails[0]);
                        this.defineRelationshipService.updateDisclosureConflictStatus(res.disclConflictStatusType);
                    }, (error: any) => {
                        if (error.status === 405) {
                            this._coiService.concurrentUpdateAction = 'Modify Conflict';
                        } else {
                            this._commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
                        }
                    }));
            // this._commonService.removeLoaderRestriction();
        }
    }

    private updateSfiDetails(sfiDetails: CoiDisclEntProjDetail, res?: ProjectSfiRelationConflictRO): void {
        sfiDetails.isDataChanged = false;
        sfiDetails.prePersonEntityId = sfiDetails.personEntityId;
        sfiDetails.disclComment.commentId = res ? res.commentId : sfiDetails.disclComment.commentId;
        const STATUS_TYPE = this.defineRelationshipService.coiStatusList.find((type: CoiProjConflictStatusType) =>
            type.projectConflictStatusCode === sfiDetails.projectConflictStatusCode
        );
        sfiDetails.coiProjConflictStatusType = deepCloneObject(STATUS_TYPE);
        const { conflictCount, conflictCompleted, conflictStatusCode, conflictStatus } = this.defineRelationshipService.getFormattedConflictData(this.projectSfiRelation.coiDisclEntProjDetails);
        this.projectSfiRelation.conflictCount = conflictCount;
        this.projectSfiRelation.conflictStatus = conflictStatus;
        this.projectSfiRelation.conflictCompleted = conflictCompleted;
        this.projectSfiRelation.conflictStatusCode = conflictStatusCode;
        this._defineRelationshipDataStore.updateOrReplaceProject(this.projectSfiRelation, ['conflictCount', 'conflictCompleted', 'conflictStatus', 'conflictStatusCode']);
        this._defineRelationshipDataStore.updateCoiDisclEntProjDetails(this.projectSfiRelation.projectId, sfiDetails);
    }

    private getProjectSfiRelationConflictRO(sfiDetails: CoiDisclEntProjDetail): ProjectSfiRelationConflictRO {
        return new ProjectSfiRelationConflictRO({
            applyAll: false,
            relationshipSFIMode: false,
            comment: sfiDetails.disclComment.comment,
            personEntityId: sfiDetails.personEntityId,
            commentId: sfiDetails.disclComment.commentId,
            coiDisclProjectId: sfiDetails.coiDisclProjectId,
            personId: this.coiData.coiDisclosure.person.personId,
            disclosureId: this.coiData.coiDisclosure.disclosureId,
            disclosureNumber: this.coiData.coiDisclosure.disclosureNumber,
            projectConflictStatusCode: sfiDetails.projectConflictStatusCode,
            coiDisclProjectEntityRelId: sfiDetails.coiDisclProjectEntityRelId
        });
    }

    onSortClick(key: 'entityName' | 'projectConflictStatusCode', parentKey: 'personEntity' | '' = ''): void {
        const currentSortDirection = this.isDesc[key] ? true : false;

        if (this.currentSortStateKey && this.currentSortStateKey !== key) {
            this.isDesc[this.currentSortStateKey] = null;
        }

        this.currentSortStateKey = key;
        this.sortList(currentSortDirection, key, parentKey);
    }

    sortList(isAsc: boolean, key: 'entityName' | 'projectConflictStatusCode', parentKey: 'personEntity' | '' = ''): void {
        this.isDesc[key] = !isAsc;

        const allKeysHaveValues = this.projectSfiRelation.coiDisclEntProjDetails.every((coiDisclEntProjDetail: CoiDisclEntProjDetail) => {
            if (parentKey) {
                return coiDisclEntProjDetail[parentKey][key];
            } else {
                return coiDisclEntProjDetail[key];
            }
        });

        if (allKeysHaveValues) {
            this.projectSfiRelation.coiDisclEntProjDetails.sort((a, b) => {
                const valA = parentKey ? a[parentKey][key] : a[key];
                const valB = parentKey ? b[parentKey][key] : b[key];

                let comparison = 0;
                if (typeof valA === 'boolean' && typeof valB === 'boolean') {
                    comparison = valA === valB ? 0 : valB ? 1 : -1;
                } else if (typeof valA === 'string' && typeof valB === 'string') {
                    comparison = valA.localeCompare(valB);
                }
                return isAsc ? comparison : -comparison;
            });
            this._cdr.markForCheck();
        }
    }

    openReviewerComment(coiDisclEntProjDetail: CoiDisclEntProjDetail): void {
        this.defineRelationshipService.openReviewerComment(this.projectSfiRelation, 'SFI', coiDisclEntProjDetail);
    }

    openAddConflictSlider(coiDisclEntProjDetail: CoiDisclEntProjDetail): void {
        this.defineRelationshipService.addConflictSlider = {
            isOpenSlider: true,
            coiDisclEntProjDetail: coiDisclEntProjDetail,
            projectSfiRelations: this.projectSfiRelation
        }
    }

}
