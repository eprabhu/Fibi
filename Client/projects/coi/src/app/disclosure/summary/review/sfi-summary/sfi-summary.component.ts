import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { CoiService } from '../../../services/coi.service';
import { CoiSummaryService } from '../../coi-summary.service';
import { environment } from '../../../../../environments/environment';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import { CommentConfiguration } from '../../../coi-interface';
import {subscriptionHandler} from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import { Router } from '@angular/router';


@Component({
    selector: 'app-sfi-summary',
    templateUrl: './sfi-summary.component.html',
    styleUrls: ['./sfi-summary.component.css']
})
export class SfiSummaryComponent implements OnInit, OnDestroy {

    coiFinancialEntityDetails: any[] = [];
    $subscriptions: Subscription[] = [];
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    coiDetails: any = {};
    searchText: string;
    isCollapsed = true;

    constructor(
        private _coiSummaryService: CoiSummaryService,
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _router: Router
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.getSfiDetails();
        this.commentConfiguration.coiSectionsTypeCode = 2;
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    getSfiDetails() {
        this.$subscriptions.push(this._coiSummaryService.getSfiDetails({
            disclosureId: Number(this.coiDetails.disclosureId),
            disclosureStatusCode: this.coiDetails.disclosureStatusCode,
            personId: this.coiDetails.personId
        }).subscribe((data: any) => {
            if (data) {
                this.coiFinancialEntityDetails = data;
                this.setSubSectionList();
            }
        }));
    }

    setSubSectionList() {
        this.commentConfiguration.subSectionList = this.coiFinancialEntityDetails.map(ele => {
            return {
                coiSubSectionsCode: ele.coiFinancialEntityId,
                description: ele.coiEntity.coiEntityName,
                coiEntity: ele.coiEntity
            };
        });
    }

    modifyReviewComment(isSubSectionComment = false, subSectionCode = null) {
        this.commentConfiguration.isSubSectionComment = isSubSectionComment;
        this.commentConfiguration.coiSubSectionsId = subSectionCode;
        this._dataStoreAndEventsService.modifyReviewComment(this.commentConfiguration);
    }

    openSfiDetails(condition: boolean, entityId: number) {
        this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: '104',mode:'view' }});
    }

}
