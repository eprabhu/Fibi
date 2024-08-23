import { Component, OnInit, Input, ChangeDetectionStrategy, OnDestroy, ChangeDetectorRef } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { DataStoreService } from '../../services/data-store.service';
import { focusElementById, openCommonModal } from '../../../common/utilities/custom-utilities';
import { isEmptyObject } from '../../../../../../../projects/fibi/src/app/common/utilities/custom-utilities';
import { DefineRelationshipService } from '../services/define-relationship.service';
import { COI, CoiDisclEntProjDetail, CoiProjConflictStatusType, DefineRelationshipDataStore, ProjectSfiRelationConflictRO, ProjectSfiRelations } from '../../coi-interface';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS, CONFLICT_STATUS_BADGE } from '../../../app-constants';
import { DefineRelationshipDataStoreService } from '../services/define-relationship-data-store.service';
@Component({
    selector: 'app-project-sfi-conflict',
    templateUrl: './project-sfi-conflict.component.html',
    styleUrls: ['./project-sfi-conflict.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class ProjectSfiConflictComponent implements OnInit, OnDestroy {

    @Input() isEditMode = false;
    @Input() projectSfiRelation = new ProjectSfiRelations();

    coiData = new COI();
    isTableCollapse: boolean[] = [];
    $subscriptions: Subscription[] = [];
    isRelationshipExpanded: boolean[] = [];
    tableSort = { isSFISortDesc: false, isConflictSortDesc: false };
    mandatoryList: Map<string, string> = new Map();
    CONFLICT_STATUS_BADGE = CONFLICT_STATUS_BADGE;
    helpText = `Click 'Apply to All' to update the Conflict of Interest (COI) status and description simultaneously for all Project SFI relationships.`;

    constructor(private _commonService: CommonService,
        public defineRelationService: DefineRelationshipService,
        private _dataStore: DataStoreService, private _cdr: ChangeDetectorRef,
        private _defineRelationsDataStore: DefineRelationshipDataStoreService) { }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.listenDataChangeFromRelationStore();
        this.processProjectSFIDetails();
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
            this._defineRelationsDataStore.relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.projectId == 'All' || changes.projectId == this.projectSfiRelation.projectId) {
                    this._cdr.markForCheck();
                }
            })
        );
    }

    private processProjectSFIDetails(): void {
        const RELATION_TYPE_MAP: { [key: string]: any } = {};
        const ICON_MAP: { [key: string]: any } = {};
    
        this.projectSfiRelation?.coiDisclEntProjDetails?.forEach((entity: CoiDisclEntProjDetail) => {
            entity.projectConflictStatusCode = entity.projectConflictStatusCode ? entity.projectConflictStatusCode : null;
            const PERSON_ENTITY = entity?.personEntity;
            const ENTITY_RELATION_TYPE = PERSON_ENTITY?.validPersonEntityRelType;
    
            if (!PERSON_ENTITY || !ENTITY_RELATION_TYPE) {
                return;
            }
    
            // Fetch or create the personEntityRelations based on ENTITY_RELATION_TYPE
            if (!RELATION_TYPE_MAP[ENTITY_RELATION_TYPE]) {
                RELATION_TYPE_MAP[ENTITY_RELATION_TYPE] = this._commonService.getEntityRelationTypePills(ENTITY_RELATION_TYPE);
            }
    
            PERSON_ENTITY.personEntityRelations = RELATION_TYPE_MAP[ENTITY_RELATION_TYPE];
    
            PERSON_ENTITY.personEntityRelations.forEach((entityRelation: any) => {
                const RELATIONSHIP_TYPE = entityRelation.relationshipType;
    
                // Fetch or create the icon for the RELATIONSHIP_TYPE
                if (!ICON_MAP[RELATIONSHIP_TYPE]) {
                    ICON_MAP[RELATIONSHIP_TYPE] = this._commonService.getRelationshipIcon(RELATIONSHIP_TYPE);
                }
    
                entityRelation.icon = ICON_MAP[RELATIONSHIP_TYPE];
            });
        });
    }
    
    private validateProjectSfiConflict(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): boolean {
        const IS_VALID_CONFLICT_STATUS = this.validateConflictStatus(sfiDetails, sfiIndex);
        const IS_VALID_CONFLICT_COMMENT = this.validateDescription(sfiDetails, sfiIndex);
        this.focusElementIfError(IS_VALID_CONFLICT_STATUS, IS_VALID_CONFLICT_COMMENT, sfiIndex);
        return IS_VALID_CONFLICT_STATUS && IS_VALID_CONFLICT_COMMENT;
    }

    private validateConflictStatus(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): boolean {
        this.mandatoryList.delete('CONFLICT_STATUS_' + sfiIndex);
        if (!sfiDetails.projectConflictStatusCode) {
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
        this.defineRelationService.applyToAllModal.isOpenModal = true;
        this.defineRelationService.applyToAllModal.coiDisclProjectId = this.projectSfiRelation.coiDisclProjectId;
        setTimeout(() => {
            openCommonModal('coi-relation-modal');
        });
    }

    validateDescription(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): boolean {
        this.mandatoryList.delete('CONFLICT_COMMENT_' + sfiIndex);
        if (!sfiDetails.disclComment.comment) {
            this.mandatoryList.set('CONFLICT_COMMENT_' + sfiIndex, 'Please enter the description.');
        }
        return !this.mandatoryList.has('CONFLICT_COMMENT_' + sfiIndex);
    }

    saveProjectSfiConflict(sfiDetails: CoiDisclEntProjDetail, sfiIndex: number): void {
        if (this.validateProjectSfiConflict(sfiDetails, sfiIndex)) {
            this.$subscriptions.push(
                this.defineRelationService.saveProjectSfiConflict(this.getProjectSfiRelationConflictRO(sfiDetails))
                    .subscribe((res: ProjectSfiRelationConflictRO[]) => {
                        this.updateSfiDetails(sfiDetails, res[0]);
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict status saved successfully.');
                    }, (_error: any) => {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                    }));
        }
    }

    private updateSfiDetails(sfiDetails: CoiDisclEntProjDetail, res: ProjectSfiRelationConflictRO): void {
        sfiDetails.disclComment.commentId = res.commentId;
        sfiDetails.coiProjConflictStatusType = this.defineRelationService.coiStatusList.find((type: CoiProjConflictStatusType) => {
            type.projectConflictStatusCode === sfiDetails.projectConflictStatusCode
        });
        this.projectSfiRelation.conflictCount = this.defineRelationService.getFormattedConflictCount(this.projectSfiRelation?.coiDisclEntProjDetails);
        this._defineRelationsDataStore.updateOrReplaceProject(this.projectSfiRelation, ['conflictCount']);
        this._defineRelationsDataStore.updateCoiDisclEntProjDetails(this.projectSfiRelation.projectId, sfiDetails);
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

}
