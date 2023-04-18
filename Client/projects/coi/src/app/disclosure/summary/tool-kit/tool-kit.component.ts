import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs/internal/Subscription';

import { CommonService } from '../../../common/services/common.service';
import { COISection, Section } from '../coi-comparison-constants';
import { CoiSummaryEventsAndStoreService } from '../coi-summary-events-and-store.service';
import { CoiSummaryService } from '../coi-summary.service';
import {slideHorizontal} from "../../../../../../fibi/src/app/common/utilities/animations";
import { scrollIntoView } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import {subscriptionHandler} from "../../../../../../fibi/src/app/common/utilities/subscription-handler";
import {HTTP_ERROR_STATUS} from "../../../../../../fibi/src/app/app-constants";

@Component({
    selector: 'app-tool-kit',
    templateUrl: './tool-kit.component.html',
    styleUrls: ['./tool-kit.component.scss'],
    animations: [slideHorizontal]
})
export class ToolKitComponent implements OnInit, OnDestroy {

    isCurrentReviewTab = 'SECTION';
    isToolkitVisible = true;
    sections: Array<Section> = COISection;
    scrollIntoView = scrollIntoView;
    projectList: any = [];
    activeProject = 1;
    coiDetails: any = {};
    proposalIdLinkedInDisclosure: number = null;

    $subscriptions: Subscription[] = [];

    constructor(
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _coiSummaryService: CoiSummaryService,
        private _commonService: CommonService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.getProjectRelationshipList();
        this.getToolkitVisibility();
    }

    getProjectRelationshipList() {
        const RELATION_SECTION = this.sections.find(section => section.reviewSectionCode === 803);
        // if (this.coiDetails.
        //     coiDisclosureCategoryType.disclosureCategoryTypeCode === '3' && this.proposalIdLinkedInDisclosure) {
        //     RELATION_SECTION.isShowProjectList = false;
        //     RELATION_SECTION.isExpanded = false;
        //     this.openProjectRelationships({
        //         moduleCode: 3,
        //         moduleItemId: this.proposalIdLinkedInDisclosure,
        //         proposalIdlinkedInDisclosure: this.proposalIdLinkedInDisclosure
        //     }, 0);
        // } else {
            RELATION_SECTION.isShowProjectList = true;
            RELATION_SECTION.isExpanded = true;
            this.getProjectRelationships();
        // }
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private fetchCOIDetails(): void {
        const DATA = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure', 'proposalIdlinkedInDisclosure']);
        this.coiDetails = DATA.coiDisclosure;
        this.proposalIdLinkedInDisclosure = DATA.proposalIdlinkedInDisclosure ? DATA.proposalIdlinkedInDisclosure : null;
    }

    getToolkitVisibility() {
        this.$subscriptions.push(this._dataStoreAndEventsService.$isToolkitVisible.subscribe(data => {
            this.isToolkitVisible = data;
            this.expandCollapseToolKit();
        }));
    }

    expandCollapseToolKit() {
        (document.getElementById('coi_summary_container') as HTMLElement).style.width = this.isToolkitVisible ? '75%' : '100%';
    }

    updateToolkitView(): void {
        this.isToolkitVisible = !this.isToolkitVisible;
        this._dataStoreAndEventsService.$isToolkitVisible.next(this.isToolkitVisible);
    }

    getProjectRelationships() {
        this._coiSummaryService.getProjectRelationships({
            disclosureId: Number(this.coiDetails.disclosureId),
            disclosureStatusCode: this.coiDetails.disclosureStatusCode,
            personId: this.coiDetails.personId,
        }).subscribe((data: any) => {
            this.projectList = data;
            this._dataStoreAndEventsService.conflictStatusList = data.coiDisclosureDetailStatuses;
            this._dataStoreAndEventsService.concatenatedProjectList = [...data.awards,...data.proposals];
            if (this.projectList.proposals.length || this.projectList.awards.length) {
                this.openProjectRelationships(this.projectList.awards[0] ?
                  this.projectList.awards[0] : this.projectList.proposals[0], 0);
            }
        }, _err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching project list');
        });
    }

    openProjectRelationships(projectDetails: any, index) {
        this.activeProject = index;
        this._dataStoreAndEventsService.$projectDetails.next({...projectDetails, INDEX: index});
    }

}
