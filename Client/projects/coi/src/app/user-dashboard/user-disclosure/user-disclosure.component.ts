import { Component, OnDestroy, OnInit } from '@angular/core';
import { UserDisclosureService } from './user-disclosure.service';
import { UserDashboardService } from '../user-dashboard.service';
import { CommonService } from '../../common/services/common.service';
import {
    CREATE_DISCLOSURE_ROUTE_URL, POST_CREATE_DISCLOSURE_ROUTE_URL, CONSULTING_REDIRECT_URL,
    CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, OPA_REDIRECT_URL,
    HTTP_ERROR_STATUS,
    DISCLOSURE_TYPE
} from '../../app-constants';
import { Router } from '@angular/router';
import { UserDisclosure } from './user-disclosure-interface';
import { Subject, interval } from 'rxjs';
import { debounce, switchMap } from 'rxjs/operators';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { listAnimation, leftSlideInOut } from '../../common/utilities/animations';
import { getDuration } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import { HeaderService } from '../../common/header/header.service';
import { getPersonLeadUnitDetails, openCoiSlider, closeSlider } from '../../common/utilities/custom-utilities';
import { COICountModal, COICountModalViewSlider } from '../../shared-components/shared-interface';

@Component({
    selector: 'app-user-disclosure',
    templateUrl: './user-disclosure.component.html',
    styleUrls: ['./user-disclosure.component.scss'],
    animations: [listAnimation, leftSlideInOut]

})

export class UserDisclosureComponent implements OnInit, OnDestroy {

    searchText = '';
    currentSelected = {
        tab: 'IN_PROGRESS_DISCLOSURES',
        filter: 'ALL',
    };
    dashboardRequestObject = {
        tabName: 'IN_PROGRESS_DISCLOSURES',
        isDownload: false,
        filterType: 'ALL',
        pageNumber: 20,
        currentPage: 1,
        property2: ''
    };
    completeDisclosureList: UserDisclosure[] = [];
    dashboardCount: any;
    isActiveDisclosureAvailable = false;
    coiList: [];
    isHover: [] = [];
    onButtonHovering: any = true;
    index: any;
    result: any;
    $subscriptions = [];
    $debounceEventForDisclosureList = new Subject();
    $fetchDisclosures = new Subject();
    isSearchTextHover = false;
    isLoading = false;
    readMoreOrLess = [];
    isShowFilterAndSearch = false;
    isShowCreate = false;
    showSlider = false;
    entityId: any;
    differenceInDays: any;
    dateWarningColor: any = false;
    dateWarning: any = true;
    hasPendingFCOI: any = false;
    hasActiveFCOI = false;
    hasPendingOPA = false;
    hasActiveOPA = false;
    completeDisclosureListCopy: any = []; /* Excat copy of original list which is to perform every array operations */
    DATA_PER_PAGE: number = 20; /* Number of data to be shown in single page */
    paginationArray: any = []; /* Introduced to set the page count after searching with some keyword */
    sliderElementId: string = '';
    isPurposeRead = {};
    $debounceEventForAPISearch = new Subject();
    pageCountFromAPI: number;
    countModalData = new COICountModal();

    constructor(public userDisclosureService: UserDisclosureService,
                public userDashboardService: UserDashboardService,
                public commonService: CommonService,
                public headerService: HeaderService,
                private _router: Router) {
    }
    ngOnInit() {
        this.loadDashboard();
        this.getTravelSearchList();
        this.getDashboardBasedOnTab();
        this.loadDashboardCount();
        window.scrollTo(0,0);
    }

    private getTravelSearchList(): void {
        this.$subscriptions.push(this.$debounceEventForAPISearch.pipe(debounce(() => interval(800))).subscribe((data: any) => {
            this.$fetchDisclosures.next();
        }));
    }

    private loadDashboard(): void {
        this.isLoading = true;
        this.$subscriptions.push(this.$fetchDisclosures.pipe(
            switchMap(() => {
                this.isLoading = true;
                this.dashboardRequestObject.property2 = this.isCurrentTabTravelOrConsulting() ? this.searchText?.trim() : '';
                return this.userDisclosureService.getCOIDashboard(this.dashboardRequestObject)
            })).subscribe((res: any) => {
                this.result = res;
                if (this.result) {
                    this.completeDisclosureList = this.getDashboardList();
                    this.completeDisclosureListCopy = this.paginationArray = JSON.parse(JSON.stringify(this.completeDisclosureList));
                    this.isCurrentTabTravelOrConsulting() ? this.pageCountFromAPI = this.result.totalServiceRequest : this.getArrayListForPagination();
                    this.loadingComplete();
                }
            }), (err) => {
                this.loadingComplete();
        });
    }

    /**
     * Description
     * @returns {any}
     * Here the sorting is applied for the merged array inorder to get the list based on the decreasing
     * order of updateTimeStamp. If any action performed on a particular disclosure or if any disclosure
     * created then that one will comes first in the list.
     */
    private getDashboardList(): any {
        const DISCLOSURE_VIEWS = this.getDisclosureProjectHeader(this.result.disclosureViews);
        const TRAVEL_DASHBOARD_VIEWS = this.result.travelDashboardViews || [];
        const OPA_DETAILS = this.result.opaDashboardDto || [];
        const CONSULTING_DISCLOSURE = this.result.consultingDisclDashboardViews || [];
        const MERGED_LIST = [...DISCLOSURE_VIEWS, ...TRAVEL_DASHBOARD_VIEWS, ...OPA_DETAILS, ...CONSULTING_DISCLOSURE];
        return this.getSortedListForParam(MERGED_LIST, 'updateTimeStamp');
    }

    private loadingComplete() {
        this.isLoading = false;
    }

    /**
     * Description
     * @param {any} arrayList:any
     * @param {any} sortByParam:any
     * @returns {any}
     * The method takes an input array and returns the corresponding sorted array for the param passed
     */
    private getSortedListForParam(arrayList: any, sortByParam: any): any {
        return arrayList.sort((a, b) => b[sortByParam] - a[sortByParam]);
    }

    private getDashboardBasedOnTab(): void {
        if (this.currentSelected.tab === 'DISCLOSURE_HISTORY') {
            this.getDisclosureHistory();
        } else {
            this.completeDisclosureList = [];
            this.$fetchDisclosures.next();
        }
    }

    /**
     * Description
     * @returns {any}
     * The function will trigger for the close button in the search field
     */
    private resetAndFetchDisclosure(): void {
        this.searchText = '';
        this.pageCountFromAPI = 0;
        this.completeDisclosureList = [];
        this.getDashboardBasedOnTab();
    }

    /**
     * Description
     * @param {any} event:number
     * @returns {any}
     * Basically for every page change, it will computes the data to be shown in that particular page by slicing the original disclosure
     * list based on the start and end indices.
     * For page 1 => data will shows from index 0 to index 19.
     * For page 2 => data will shows from index 20 to index 39 and so on
     */
    actionsOnPageChange(event: number): void {
        if (this.dashboardRequestObject.currentPage != event) {
            this.dashboardRequestObject.currentPage = event;
            this.isCurrentTabTravelOrConsulting() ? this.$fetchDisclosures.next() : this.getArrayListForPagination();
        }
    }

    /**
     * Description
     * @returns {any}
     * Arranges data in each page according to the Maximum number of data. By default the maximum number of data shows in a page is 20.
     * This function is implemented on purpose as we are removing the pagination functionality from server side. This is because we won't
     * be having a huge data in the dashboard in real time(production environment). Hence we dont want to make unnecessary api calls to
     * the server everytime for fetching the data.
     */
    private getArrayListForPagination(): void {
        const [START_INDEX, END_INDEX] = [this.getStartIndex(), this.getEndIndex()];
        this.completeDisclosureList = this.completeDisclosureListCopy.slice(START_INDEX, END_INDEX + 1);
    }

    /**
     * Description
     * @returns {any}
     * If there is only one page, then the maximum number of data would be 20. so we need to arrange the data from [0 to 19].
     * i.e., the starting point would always be 0 and ending point is always 19. Otherwise, for instance if the user clicks
     * on 2nd page, the starting point would be (2-1) * 20 = 20.
     */
    private getStartIndex(): number {
        if (this.dashboardRequestObject.currentPage == 1) { return 0; }
        return (this.dashboardRequestObject.currentPage - 1) * this.DATA_PER_PAGE;
    }

    /**
     * Description
     * @returns {any}
     * If there is only one page, then the maximum number of data would be 20. so we need to arrange the data from [0 to 19].
     * i.e., the starting point would always be 0 and ending point is always 19. Otherwise, for instance if the user clicks
     * on 2nd page, the ending point would be (20 * 2) - 1  = 39.
     */
    private getEndIndex(): number {
        if (this.dashboardRequestObject.currentPage == 1) { return this.DATA_PER_PAGE - 1; }
        return (this.DATA_PER_PAGE * this.dashboardRequestObject.currentPage) - 1;
    }

    /**
     * This function handles the filtering of disclosure lists based on the search word.
     *
     * Note:
     * - For the 'TRAVEL_DISCLOSURES' tab, pagination and search are managed by the backend,
     *   so a debounce event is triggered to handle this.
     * - For all other tabs, the search and pagination is performed on the frontend by filtering the local
     *   completeDisclosureListCopy array.
     *
     * The pagination is reset to the first page whenever a search is initiated.
     */
    getFilteredDisclosureListForSearchWord(): any {
        this.dashboardRequestObject.currentPage = 1; /* To set the pagination while search */
        if (this.isCurrentTabTravelOrConsulting()) {
            this.$debounceEventForAPISearch.next();
        } else {
            this.completeDisclosureList = this.completeDisclosureListCopy.filter(disclosure => {
                for (const value in disclosure) {
                    if (this.isExistSearchWord(disclosure, value)) { return true; }
                }
                return false;
            });
            this.resetDisclosureCopy();
        }
    }

    private isExistSearchWord(disclosure: any, value: string): boolean {
        if (disclosure[value]?.unitDetail) {
            return disclosure[value].unitDetail.toString().trim().toLowerCase().includes(this.searchText.trim().toLowerCase());
        }
        return disclosure[value] && disclosure[value].toString().trim().toLowerCase().includes(this.searchText.trim().toLowerCase());
    }

    resetDashboardAfterSearch(): void {
        this.searchText = '';
        if(this.isCurrentTabTravelOrConsulting()) {
            this.$fetchDisclosures.next();
        } else {
            this.completeDisclosureList = this.getDashboardList();
            this.resetDisclosureCopy();
            this.getArrayListForPagination();
        }
    }

    isCurrentTabTravelOrConsulting() {
        return ['TRAVEL_DISCLOSURES', 'CONSULTING_DISCLOSURES'].includes(this.currentSelected.tab);
    }

    /**
     * Description
     * @returns {any}
     * The function is to set the pagination array separately while searching with a keyword
     */
    private resetDisclosureCopy(): void {
        this.paginationArray = this.completeDisclosureList;
    }

    private loadDashboardCount() {
        this.userDisclosureService.getCOIDashboardCount(this.dashboardRequestObject).subscribe((res: any) => {
            this.dashboardCount = res;
            this.isShowFilterAndSearch = !!res?.inProgressDisclosureCount;
            this.setIsShowCreateFlag();
        });
    }

    private setIsShowCreateFlag() {
        if (!this.dashboardCount.inProgressDisclosureCount && !this.dashboardCount.approvedDisclosureCount
            && !this.dashboardCount.travelDisclosureCount && !this.dashboardCount.disclosureHistoryCount &&
            !this.completeDisclosureList.length &&
            this.dashboardRequestObject.currentPage == 1 && this.dashboardRequestObject.filterType == 'ALL') {
            this.isShowCreate = true;
        }
    }

    getEventType(disclosureSequenceStatusCode, disclosureCategoryType) {
        if (disclosureCategoryType === 1) {
            if (disclosureSequenceStatusCode === 2 || disclosureSequenceStatusCode === 1 && !this.isActiveDisclosureAvailable) {
                return 'Active';
            } else if (disclosureSequenceStatusCode === 1 && this.isActiveDisclosureAvailable) {
                return 'Revision';
            }
        } else if (disclosureCategoryType === 3) {
            return 'Proposal';
        }
    }

    setTab(tabName: string, disclosureCount: number = 0) {
        this.currentSelected.tab = tabName;
        this.dashboardRequestObject.tabName = tabName;
        this.dashboardRequestObject.currentPage = 1;
        this.dashboardRequestObject.filterType = 'ALL';
        this.currentSelected.filter = 'ALL';
        this.isShowFilterAndSearch = !!disclosureCount;
        this.resetAndFetchDisclosure();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    openCountModal(disclosure: any, count: number | null = null, moduleCode: number = 0) {
        if (count > 0) {
            this.countModalData = {
                personUnit: disclosure?.unit,
                moduleCode: moduleCode,
                personId: disclosure?.personId,
                disclosureType: disclosure?.fcoiTypeCode === '2' ? disclosure?.projectType : 'FCOI',
                inputType: 'DISCLOSURE_TAB',
                fcoiTypeCode: disclosure?.fcoiTypeCode,
                disclosureId: disclosure?.coiDisclosureId,
                personFullName: disclosure?.disclosurePersonFullName,
                projectHeader: disclosure?.projectHeader,
                isOpenCountModal: true
            };
        }
    }

    setFilter(type = 'ALL') {
        this.currentSelected.filter = type;
        this.dashboardRequestObject.filterType = type;
        this.dashboardRequestObject.currentPage = 1;
        this.resetAndFetchDisclosure();
    }

    redirectToDisclosure(disclosure: UserDisclosure) {
        let redirectUrl;
        if (disclosure.travelDisclosureId) {
            const isTravelDisclosureEditPage = ['1', '4', '5'].includes(disclosure.reviewStatusCode);
            redirectUrl = isTravelDisclosureEditPage ? CREATE_TRAVEL_DISCLOSURE_ROUTE_URL : POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL;
        } else if (disclosure.opaDisclosureId) {
            redirectUrl = OPA_REDIRECT_URL;
        } else if(disclosure.disclosureId) {
            redirectUrl = CONSULTING_REDIRECT_URL;
        }
        else {
            const isDisclosureEditPage = ['1', '5', '6'].includes(disclosure.reviewStatusCode);
            redirectUrl = isDisclosureEditPage ? CREATE_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL;
        }
        this._router.navigate([redirectUrl],
            { queryParams: { disclosureId: disclosure.travelDisclosureId || disclosure.coiDisclosureId || disclosure.opaDisclosureId || disclosure.disclosureId} });
    }

    redirectToFCOIDisclosure(disclosures: any) {
        if (disclosures?.disclosureId) {
            const IS_DISCLOSURE_EDITABLE  = ['1', '5', '6'].includes(disclosures?.reviewStatusCode);
            const REDIRECT_URL = IS_DISCLOSURE_EDITABLE ? CREATE_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL;
            this._router.navigate([REDIRECT_URL], { queryParams: { disclosureId: disclosures?.disclosureId} });
        }
    }

    formatTravellerTypes(travellerTypes: string): string {
        return travellerTypes ? (travellerTypes.split(',').map(travellerType => travellerType.trim()).join(', ')) : '';
    }

    getSearchPlaceHolder() {
        if (this.currentSelected.tab === 'TRAVEL_DISCLOSURES') {
            return 'Search by Entity Name, Department, Traveller Type, Destination, Review Status, Document Status, Purpose';
        } else if(this.currentSelected.tab === 'CONSULTING_DISCLOSURES') {
            return 'Search by Entity Name, Department, Disposition Status, Review Status';
        } else {
            return 'Search by Project Title, Department, Disclosure Status, Disposition Status, Review Status';
        }
    }

    private getDisclosureHistory() {
        this.isLoading = true;
        this.completeDisclosureList = [];
        this.$subscriptions.push(this.userDisclosureService.getDisclosureHistory({ 'filterType': this.currentSelected.filter }).subscribe((data: any) => {
            this.completeDisclosureList = this.getAllDisclosureHistories(data);;
            this.loadingComplete();
        }));
    }

    private getAllDisclosureHistories(data: any): any {
        const DISCLOSURE_HISTORY = this.getDisclosureHistoryProjectHeader(data.disclosureHistoryDtos)
        const OPA_HISTORY = data.opaDashboardDtos || [];
        const MERGED_LIST = [...DISCLOSURE_HISTORY, ...OPA_HISTORY];
        return this.getSortedListForParam(MERGED_LIST, 'updateTimeStamp');
    }

    private getDisclosureHistoryProjectHeader(disclosureHistory: any): any[] {
        return disclosureHistory?.map((ele: any) => {
            ele.projectHeader = ele.fcoiTypeCode === '2' ? `#${ele.projectNumber} - ${ele.projectTitle}` : '';
            return ele;
        }) || [];
    }

    private getDisclosureProjectHeader(disclosures: any) {
        return disclosures?.map((ele: any) => {
            ele.projectHeader = ele.fcoiTypeCode === '2' ? `#${ele.projectNumber} - ${ele.projectTitle}` : '';
            return ele;
        }) || [];
    }

    openFCOIModal(type) {
        this.headerService.$openModal.next(type);
    }

    viewSlider(event: COICountModalViewSlider) {
        this.showSlider = event.isOpenSlider;
        this.entityId = event.entityId;
        this.sliderElementId = `user-disclosure-entity-${this.entityId}`;
        setTimeout(() => {
            openCoiSlider(this.sliderElementId);
        });
    }

    validateSliderClose() {
        setTimeout(() => {
            this.showSlider = false;
            this.entityId = null;
            this.sliderElementId = '';
        }, 500);
    }

    getActiveFCOI() {
        this.fcoiDatesRemaining();
        return this.headerService.activeDisclosures.filter(disclosure =>
            ([DISCLOSURE_TYPE.FCOI, DISCLOSURE_TYPE.REVISION].includes(disclosure?.fcoiTypeCode)));
    }

    private fcoiDatesRemaining() {
        this.hasPendingFCOI = false;
        this.hasActiveFCOI = false;
        this.hasActiveOPA = false;
        this.hasPendingOPA = false;
        this.headerService.activeDisclosures.forEach(disclosure => {
            if (([DISCLOSURE_TYPE.FCOI, DISCLOSURE_TYPE.REVISION].includes(disclosure?.fcoiTypeCode)) && disclosure?.versionStatus == 'PENDING') {
                this.hasPendingFCOI = true;
            }
            if (([DISCLOSURE_TYPE.FCOI, DISCLOSURE_TYPE.REVISION].includes(disclosure?.fcoiTypeCode)) && disclosure?.versionStatus !== 'PENDING') {
                this.hasActiveFCOI = true;
            }
        });
        this.headerService.activeOPAs.forEach(disclosure => {
            if (disclosure?.dispositionStatusType?.dispositionStatusCode === '3') {
                this.hasActiveOPA = true;
            }
            if (disclosure?.dispositionStatusType?.dispositionStatusCode === '1') {
                this.hasPendingOPA = true;
            }
        });
        const disclosureDate = this.headerService.activeDisclosures.filter(disclosure =>
            ([DISCLOSURE_TYPE.FCOI, DISCLOSURE_TYPE.REVISION].includes(disclosure?.fcoiTypeCode)) && disclosure?.versionStatus !== 'PENDING');
        if (disclosureDate[0]) {
            const expirationDate = (disclosureDate[0].expirationDate);
            const currentDate = new Date().getTime();
            this.differenceInDays = getDuration(currentDate, expirationDate);
            if ((this.differenceInDays.durInDays + (this.differenceInDays.durInMonths * 30) +
                (this.differenceInDays.durInYears * 360)) < 10) {
                this.dateWarningColor = true;
            } else if ((this.differenceInDays.durInDays + (this.differenceInDays.durInMonths * 30) +
                (this.differenceInDays.durInYears * 360)) > 30) {
                this.dateWarning = false;
            }
        }
    }

    createOPA() {
        this.$subscriptions.push(this.userDisclosureService.createOPA(this.commonService.getCurrentUserDetail('personID'),
            this.commonService.getCurrentUserDetail('unitNumber'))
            .subscribe((res: any) => {
                if(res) {
                    this._router.navigate([OPA_REDIRECT_URL], { queryParams: { disclosureId: res.disclosureId } });
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.');
                }
            }, err => this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.')));
    }

    selectedTabLabel(): string {
        //Need correct label from ba
        switch (this.currentSelected.tab) {
            case 'IN_PROGRESS_DISCLOSURES':
                return 'Results for In Progress Disclosures';
            case 'APPROVED_DISCLOSURES':
                return 'Results for Approved Disclosures';
            case 'TRAVEL_DISCLOSURES':
                return 'Results for Travel Disclosures';
            case 'DISCLOSURE_HISTORY':
                return 'Results for Disclosure History';
            default:
                return '';
        }
    }

    readMorePurpose(id: number, flag: boolean): void {
        this.isPurposeRead[id] = !flag;
    }

}
