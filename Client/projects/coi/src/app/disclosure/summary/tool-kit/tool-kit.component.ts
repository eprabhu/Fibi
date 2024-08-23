import { Component, HostListener, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs/internal/Subscription';

import { CommonService } from '../../../common/services/common.service';
import { COISection, Section } from '../coi-comparison-constants';
import { CoiSummaryEventsAndStoreService } from '../services/coi-summary-events-and-store.service';
import { CoiSummaryService } from '../services/coi-summary.service';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import {HTTP_ERROR_STATUS} from '../../../../../../fibi/src/app/app-constants';
import { listAnimation, slideInAnimation } from '../../../common/utilities/animations';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { CoiService } from '../../services/coi.service';
import { ProjectRelationshipDetails } from '../../coi-interface';

@Component({
    selector: 'app-tool-kit',
    templateUrl: './tool-kit.component.html',
    styleUrls: ['./tool-kit.component.scss'],
    animations: [ listAnimation, slideHorizontal,
        slideInAnimation('0','12px', 400, 'slideUp'),
        slideInAnimation('0','-12px', 400, 'slideDown')
    ]
})
export class ToolKitComponent implements OnInit, OnDestroy {

    isCurrentReviewTab = 'SECTION';
    isToolkitVisible = true;
    sections: Array<Section> = COISection;
    projectList: ProjectRelationshipDetails[] = [];
    activeProject = 1;
    coiDetails: any = {};
    proposalIdLinkedInDisclosure: number = null;

    $subscriptions: Subscription[] = [];
    isRelationshipCollapse = false;
    activeNav = '';
    activeSubNav = '';
    awardList: ProjectRelationshipDetails[] = [];
    proposalList: ProjectRelationshipDetails[] = [];
    IPList: ProjectRelationshipDetails[] = [];

    constructor(
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        public coiSummaryService: CoiSummaryService,
        private _commonService: CommonService,private _coiService: CoiService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.getProjectRelationshipList();
        this.getToolkitVisibility();
        this.activeNav = 'COI801';
        this.listenScreenSize();
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
            this.getProjectRelationshipDetails();
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


    openProjectRelationships(projectDetails: any, index) {
        this.activeProject = index;
        this._dataStoreAndEventsService.$projectDetails.next({...projectDetails, INDEX: index});
    }

    jumpToSection(section) {
        this.activeNav = section;
        if (section !== 'COI803') {
            this.activeSubNav = '';
            this.coiSummaryService.activeSubNavItemId = '';
            this.isRelationshipCollapse = false; 
        } else {
            this.isRelationshipCollapse = !this.isRelationshipCollapse;
            this.coiSummaryService.activeSubNavItemId = '';
        }
        this.openCollapsedSection(section);
        this.listenScreenSize();
        this.windowScroll(section);
    }

    jumpToProjectSection(parentSection: string, activeSubSection: string, projectTypeCode: string, projectId: string) {
        this.openCollapsedSection(parentSection);
        this.activeNav = parentSection;
        this.activeSubNav = activeSubSection.toUpperCase();
        this.coiSummaryService.activeSubNavItemId = projectTypeCode + '-' + projectId;
        setTimeout(() => {
            this.windowScroll(this.coiSummaryService.activeSubNavItemId);
        });
    }

    windowScroll(scrollTo: string) {
        const ELEMENT: HTMLElement = document.getElementById(scrollTo);
        const offsetFromHeader = document.getElementById('COI-DISCLOSURE-HEADER')?.clientHeight + 50;
        const sectionHeight = ELEMENT.offsetTop - offsetFromHeader;
        window.scrollTo({ behavior: 'smooth', top: sectionHeight });
    }

    private openCollapsedSection(section) {
        this._coiService.$isExpandSection.next({ section: section, isExpand: true });
    }

    @HostListener('window:resize', ['$event'])
    listenScreenSize() {
        setTimeout(() => {
            const WINDOW_HEIGHT = window.innerHeight;
            const HEADER_HEIGHT = document.getElementById('COI-DISCLOSURE-HEADER')?.offsetHeight || 0;
            const TOOL_KIT_HEIGHT = WINDOW_HEIGHT - (HEADER_HEIGHT + 60);
            document.getElementById('disclosure-toolkit').style.maxHeight = TOOL_KIT_HEIGHT + 'px';
            if (this.isRelationshipCollapse && document.getElementById('relationship-chid-section')) {
                const RELATIONSHIP_HEIGHT = TOOL_KIT_HEIGHT / 4;
                document.getElementById('relationship-chid-section').style.maxHeight = RELATIONSHIP_HEIGHT + 'px';
            }
        });
    }

    private getProjectRelationshipDetails() {
        this.$subscriptions.push(this._coiService.getDisclosureProjectList(this.coiDetails.disclosureId).subscribe((res: ProjectRelationshipDetails[]) => {
            this.projectList = res || [];
            this._dataStoreAndEventsService.concatenatedProjectList = res;
            this.awardList = this.projectList.filter(ele => ele.projectTypeCode == '1');
            this.proposalList = this.projectList.filter(ele => ele.projectTypeCode == '3');
            this.IPList = this.projectList.filter(ele => ele.projectTypeCode == '2');
        }, _err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching project list');
        }));
    }
}
