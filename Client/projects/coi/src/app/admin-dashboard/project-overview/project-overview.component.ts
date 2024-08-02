import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { ProjectOverviewService } from '../project-overview.service';
import { fadeInOutHeight, heightAnimation, leftSlideInOut, listAnimation, scaleOutAnimation, slideInAnimation, topSlideInOut } from '../../common/utilities/animations';
import { CommonService } from '../../common/services/common.service';
import { CoiProjectOverviewRequest, ProjectDetails, ProjectOverview } from '../admin-dashboard.interface';
import { POST_CREATE_DISCLOSURE_ROUTE_URL } from '../../app-constants';
import { Router } from '@angular/router';
import { Observable, Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { getFormattedSponsor } from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-project-overview',
    templateUrl: './project-overview.component.html',
    styleUrls: ['./project-overview.component.scss'],
    providers: [ProjectOverviewService],
    animations: [fadeInOutHeight, listAnimation, topSlideInOut, leftSlideInOut, heightAnimation('0', '*', 300, 'heightAnimation'),
        slideInAnimation('0', '12px', 400, 'slideUp'),
        slideInAnimation('0', '-12px', 400, 'slideDown'),
        scaleOutAnimation('-2px', '0', 200, 'scaleOut'),
    ]
})
export class ProjectOverviewComponent implements OnInit, OnDestroy {

    @Input() dataForAdvanceSearch: Observable<any>;
    projectOverviewData: ProjectOverview = new ProjectOverview();
    projectOverviewRequestObject = new CoiProjectOverviewRequest();
    displayFilterTab = false;
    isProjectOverviewCardCollapse: boolean[] = [];
    $subscriptions: Subscription[] = [];
    isLoading = true;
    currentFilter = '';
    isDesc: { [key: string]: boolean } = {};
    showSlider = false;
    private isInitialCall: boolean = true;
    totalPageCount: number | null = null;
    getIndexForSlider: number;
    getFormattedSponsor = getFormattedSponsor;


    constructor(private projectOverviewService: ProjectOverviewService, public commonService: CommonService, private _router: Router) { }

    ngOnInit(): void {
        this.loadProjectData();
        this.advSearchData();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    advSearchData(): void {
        this.$subscriptions.push(this.dataForAdvanceSearch.subscribe((data) => {
            this.projectOverviewRequestObject = data;
            this.projectOverviewData = new ProjectOverview();
            this.isInitialCall = true;
            this.loadProjectData();
        }));
    }

    public loadProjectData(): void {
        this.isLoading = true;
        this.projectOverviewRequestObject.isDownload = this.isInitialCall;
        this.isInitialCall = false;

        this.$subscriptions.push(this.projectOverviewService.getCOIProjectOverviewDetails(this.projectOverviewRequestObject).subscribe({
            next: (response: ProjectOverview) => {
                if (response.proposalCount !== undefined) {
                    this.totalPageCount = response.proposalCount;
                }
                this.projectOverviewData = response;
                this.isLoading = false;
            },
            error: () => {
                this.isLoading = false;
            }
        }));
    }

    toggleProjectOverviewCard(index: number) {
        this.isProjectOverviewCardCollapse[index] = !this.isProjectOverviewCardCollapse[index];
    }

    sortKeypersonsList(isAsc: boolean, index: number, key: any) {
        const stateKey = `${index}_${key}`;
        this.isDesc[stateKey] = !isAsc;
        const allKeysHaveValues = this.projectOverviewData.projectOverviewDetails[index].keyPersonDetails.every(person => person[key] !== undefined);

        if (allKeysHaveValues) {
            this.projectOverviewData.projectOverviewDetails[index].keyPersonDetails.sort((a, b) => {
                const valA = a[key];
                const valB = b[key];

            let comparison = 0;
            if (typeof valA === 'boolean' && typeof valB === 'boolean') {
                comparison = valA === valB ? 0 : valB ? 1 : -1;
            } else if (typeof valA === 'string' && typeof valB === 'string') {
                comparison = valA.localeCompare(valB);
            }
            return isAsc ? comparison : -comparison;
        });
        }
    }

    onSortClick(index: number, key: any) {
        const stateKey = `${index}_${key}`;
        const currentSortDirection = this.isDesc[stateKey] ? true : false;
        this.sortKeypersonsList(currentSortDirection, index, key);
    }

    redirectToDisclosure(KeyPersonData) {
        if (KeyPersonData.disclosureId) {
            const redirectUrl = POST_CREATE_DISCLOSURE_ROUTE_URL;
            this._router.navigate([redirectUrl],
                { queryParams: { disclosureId: KeyPersonData.disclosureId } });
        }
    }

    openProjectDetailsModal(projectDetails: ProjectDetails): void {
        const SELECTED_PROJECT_DETAILS = {
            title: projectDetails?.title,
            sponsorName: projectDetails?.sponsorName,
            sponsorCode : projectDetails?.sponsorCode,
            homeUnitName: projectDetails?.leadUnitName,
            projectEndDate: projectDetails?.projectEndDate,
            projectId: projectDetails?.projectId,
            homeUnitNumber: projectDetails?.leadUnitNumber,
            reporterRole: projectDetails?.reporterRole,
            projectStartDate: projectDetails?.projectStartDate,
            projectStatus: projectDetails?.projectStatus,
            projectTypeCode: projectDetails?.projectTypeCode,
            piName: projectDetails?.piName,
            primeSponsorName: projectDetails?.primeSponsorName,
            primeSponsorCode: projectDetails?.primeSponsorCode,
            projectType: projectDetails.projectType,
            projectBadgeColour: projectDetails.projectBadgeColour,
            projectNumber: projectDetails?.projectId
        }
        this.commonService.openProjectDetailsModal(SELECTED_PROJECT_DETAILS, null, false);
    }

    openPersonDetailsModal(personID: string): any {
        this.commonService.openPersonDetailsModal(personID);
    }

    getSubmissionStatusBadge(status) {
        switch (status) {
            case 'Pending':
                return 'yellow-badge';
            case 'Not Required':
                return 'blue-badge';
            case 'Completed':
                return 'green-badge';
            default:
                return 'yellow-badge';
        }
    }

    getReviewStatusBadge(status: string): string {
        switch (status) {
            case 'Pending':
                return 'yellow-badge';
            case '2':
                return 'blue-badge';
            case 'Completed':
                return 'green-badge';
            case '5':
                return 'red-badge';
            default:
                return 'red-badge';
        }
    }

    toggleSlider(index: number) {
        this.getIndexForSlider = index;
        this.showSlider = true;
    }

    closeHeaderSlider() {
        this.showSlider = false;
    }

    actionsOnPageChange(event) {
        if (this.projectOverviewRequestObject.currentPage != event) {
            this.isProjectOverviewCardCollapse = [];
            this.projectOverviewRequestObject.currentPage = event;
            this.loadProjectData();
        }
    }

    handleCommentCount(updatedCount: number) {
        this.projectOverviewData.projectOverviewDetails[this.getIndexForSlider].projectDetails.commentCount = updatedCount;
    }

    setFilter(test: string): void { }
}
