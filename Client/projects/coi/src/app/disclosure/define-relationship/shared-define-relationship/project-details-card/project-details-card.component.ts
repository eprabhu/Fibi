import { ChangeDetectionStrategy, ChangeDetectorRef, Component, Input, OnDestroy, OnInit } from '@angular/core';
import { DefineRelationshipDataStore, ProjectSfiRelations } from '../../../coi-interface';
import { DISCLOSURE_CONFLICT_STATUS_BADGE, PROJECT_DETAILS_ORDER } from '../../../../app-constants';
import { Subscription } from 'rxjs';
import { DefineRelationshipDataStoreService } from '../../services/define-relationship-data-store.service';
import { DefineRelationshipService } from '../../services/define-relationship.service';
import { getFormattedSponsor } from '../../../../common/utilities/custom-utilities';
import { CommonService } from '../../../../common/services/common.service';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';

@Component({
    selector: 'app-project-details-card',
    templateUrl: './project-details-card.component.html',
    styleUrls: ['./project-details-card.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class ProjectDetailsCardComponent implements OnInit, OnDestroy {

    @Input() projectSfiRelation = new ProjectSfiRelations();

    $subscriptions: Subscription[] = [];
    sponsor = '';
    primeSponsor = '';
    PROJECT_DETAILS_ORDER = PROJECT_DETAILS_ORDER;
    DISCLOSURE_CONFLICT_STATUS_BADGE = DISCLOSURE_CONFLICT_STATUS_BADGE;

    constructor(private _cdr: ChangeDetectorRef,
                public commonService: CommonService,
                public defineRelationshipService: DefineRelationshipService,
                private _defineRelationshipDataStore: DefineRelationshipDataStoreService,
    ) { }

    ngOnInit(): void {
        this.setSponsorAndPrimeSponsor();
        this.listenDataChangeFromRelationStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private setSponsorAndPrimeSponsor(): void {
        this.sponsor = getFormattedSponsor(this.projectSfiRelation?.sponsorCode, this.projectSfiRelation?.sponsorName);
        this.primeSponsor = getFormattedSponsor(this.projectSfiRelation?.primeSponsorCode, this.projectSfiRelation?.primeSponsorName);
    }

    private listenDataChangeFromRelationStore(): void {
        this.$subscriptions.push(
            this._defineRelationshipDataStore.$relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.projectId == 'ALL' || changes.projectId == this.projectSfiRelation.projectId) {
                    this.setSponsorAndPrimeSponsor();
                    this._cdr.markForCheck();
                }
            }));
    }

    openReviewerComment(): void {
        this.defineRelationshipService.openReviewerComment(this.projectSfiRelation, 'RELATIONSHIP');
    }

    openProjectHierarchySlider(): void {
        this.commonService.openProjectHierarchySlider(this.projectSfiRelation.moduleCode, this.projectSfiRelation.projectNumber);
    }

}
