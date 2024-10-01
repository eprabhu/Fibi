import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { ProjectOverviewService } from '../project-overview.service';
import { fadeInOutHeight, heightAnimation, leftSlideInOut, listAnimation, scaleOutAnimation, slideInAnimation, topSlideInOut } from '../../common/utilities/animations';
import { CommonService } from '../../common/services/common.service';
import { CoiProjectOverviewRequest, NotificationObject, ProjectDetails, ProjectOverview } from '../admin-dashboard.interface';
import { POST_CREATE_DISCLOSURE_ROUTE_URL, PROJECT_DETAILS_ORDER_WITHOUT_ROLE  } from '../../app-constants';
import { Router } from '@angular/router';
import { Observable, Subject, Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { getFormattedSponsor } from '../../common/utilities/custom-utilities';
@Component({
    selector: 'app-project-overview',
    templateUrl: './project-overview.component.html',
    styleUrls: ['./project-overview.component.scss'],
    providers: [ProjectOverviewService],
    animations: [listAnimation, heightAnimation('0', '*', 300, 'heightAnimation')]
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
    isShowNotificationSlider = false;
    private isInitialCall: boolean = true;
    totalPageCount: number | null = null;
    getIndexForSlider: number;
    selectedKeyPersonIndex: number;
    projectDetailsForSlider: any;
    currentSortStateKey: string | null = null;
    getFormattedSponsor = getFormattedSponsor;
    PROJECT_DETAILS_ORDER_WITHOUT_ROLE  = PROJECT_DETAILS_ORDER_WITHOUT_ROLE ;

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

    toggleProjectOverviewCard(index: number): void {
        this.isProjectOverviewCardCollapse[index] = !this.isProjectOverviewCardCollapse[index];
    }

    sortKeypersonsList(isAsc: boolean, index: number, key: any): void {
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

    /**
     * 
     * @param index To retrieve the accurate project using the index value.
     * @param key The key is used to obtain a specific field name from the keyperson table, based on the data given at a particular index value.
     */
    onSortClick(index: number, key: any): void {
        const stateKey = `${index}_${key}`;
        const currentSortDirection = this.isDesc[stateKey] ? true : false;
        
        if (this.currentSortStateKey && this.currentSortStateKey !== stateKey) {
            this.isDesc[this.currentSortStateKey] = null;
        }
    
        this.currentSortStateKey = stateKey;
        this.sortKeypersonsList(currentSortDirection, index, key);
    }

    redirectToDisclosure(KeyPersonData) {
        if (KeyPersonData.disclosureId) {
            const redirectUrl = POST_CREATE_DISCLOSURE_ROUTE_URL;
            this._router.navigate([redirectUrl],
                { queryParams: { disclosureId: KeyPersonData.disclosureId } });
        }
    }

    openProjectHierarchySlider(projectDetails: ProjectDetails): void {
        this.commonService.openProjectHierarchySlider(projectDetails?.projectTypeCode, projectDetails?.projectNumber);
    }

    openPersonDetailsModal(personID: string): any {
        this.commonService.openPersonDetailsModal(personID);
    }

    getSubmissionStatusBadge(status) {
        switch (status) {
            case 'Pending':
                return 'yellow-badge';
            case 'Not Required':
                return 'grey-badge';
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
            case 'N/A':
                return 'grey-badge';
            case 'Completed':
                return 'green-badge';
            default:
                return 'red-badge';
        }
    }

    toggleSlider(index: number): void {
        this.getIndexForSlider = index;
        this.showSlider = true;
    }

    toggleNotificationSlider(projectDetailsForSlider: number, index: number): void{
        this.selectedKeyPersonIndex = index;
        this.projectDetailsForSlider = projectDetailsForSlider;
        this.isShowNotificationSlider = true;
    }

    closeHeaderSlider(): void {
        this.showSlider = false;
        this.isShowNotificationSlider = false;
    }

    actionsOnPageChange(event): void {
        if (this.projectOverviewRequestObject.currentPage != event) {
            this.isProjectOverviewCardCollapse = [];
            this.projectOverviewRequestObject.currentPage = event;
            this.loadProjectData();
        }
    }

    handleCommentCount(updatedCount: number): void {
        this.projectOverviewData.projectOverviewDetails[this.getIndexForSlider].projectDetails.commentCount = updatedCount;
    }

    getCertificationFlagDisplay(status: string): string {
        switch (status) {
            case 'COMPLETED':
                return 'Completed';
            case 'INCOMPLETE':
                return 'Incomplete';
            case 'NOT_REQUIRED':
                return 'Not Required';
            default:
                return 'Incomplete';
        }
    }

    getDisclosureStatusDisplay(status: string): string {
        switch (status) {
            case 'Pending':
                return 'Pending';
            case 'TO_BE_DETERMINED':
                return '--';
            case 'Completed':
                return 'Completed';
            case 'Not Required':
                return 'Not Required';
            default:
                return 'Pending';
        }
    }

    setFilter(test: string): void { }
}
