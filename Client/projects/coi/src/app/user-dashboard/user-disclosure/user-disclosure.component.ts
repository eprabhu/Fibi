import { Component, ElementRef, EventEmitter, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { UserDisclosureService } from './user-disclosure.service';
import { UserDashboardService } from '../user-dashboard.service';
import { CommonService } from '../../common/services/common.service';
import { CREATE_DISCLOSURE_ROUTE_URL, POST_CREATE_DISCLOSURE_ROUTE_URL,
         CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, OPA_REDIRECT_URL } from '../../app-constants';
import { Router } from '@angular/router';
import { UserDisclosure } from './user-disclosure-interface';
import { Subject, interval } from 'rxjs';
import { debounce, switchMap } from 'rxjs/operators';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { listAnimation, leftSlideInOut } from '../../common/utilities/animations';
import { closeSlider, openSlider } from '../../common/utilities/custom-utilities';
import { getDuration } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import {HeaderService} from '../../common/header/header.service';

@Component({
    selector: 'app-user-disclosure',
    templateUrl: './user-disclosure.component.html',
    styleUrls: ['./user-disclosure.component.scss'],
    animations: [listAnimation, leftSlideInOut]

})

export class UserDisclosureComponent implements OnInit, OnDestroy {
    isShowCountModal = false;
    searchText = '';
    currentSelected = {
        tab: 'IN_PROGRESS_DISCLOSURES',
        filter: 'ALL',
    };
    dashboardRequestObject = {
        tabName: 'IN_PROGRESS_DISCLOSURES',
        isDownload: false,
        filterType: 'ALL',
        currentPage: 1
    };
    completeDisclosureList: UserDisclosure[] = [];
    dashboardCount: any;
    isActiveDisclosureAvailable = false;
    selectedModuleCode: number;
    currentDisclosureId: any;
    currentDisclosureNumber: any;
    disclosureType: any;
    inputType: string;
    coiList: [];
    isHover: [] = [];
    disclosureSequenceStatusCode: any;
    personId: any;
    onButtonHovering: any = true;
    index: any;
    fcoiTypeCode: any;
    disclosures: any;
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
    completeDisclosureListCopy: any = [];
    DATA_PER_PAGE: number = 20;
    searchResult: any = [];
    searchByList: any = ['proposalTitle', 'awardTitle', 'disclosurestatus', 'dispositionStatus', 'reviewStatus', 'conflictStatus', 'unit.unitName'];

    constructor(public userDisclosureService: UserDisclosureService,
        public userDashboardService: UserDashboardService,
        public commonService: CommonService,
        public headerService: HeaderService,
        private _router: Router) {
    }

    ngOnInit() {
        this.loadDashboard();
        this.getDashboardBasedOnTab();
        this.loadDashboardCount();
    }

    loadDashboard() {
        this.isLoading = true;
        this.$subscriptions.push(this.$fetchDisclosures.pipe(
            switchMap(() => {
                this.isLoading = true;
                return this.userDisclosureService.getCOIDashboard(this.dashboardRequestObject)
            })).subscribe((res: any) => {
                this.result = res;
                if (this.result) {
                    this.completeDisclosureList =  this.getDashboardList();
                    this.completeDisclosureListCopy = JSON.parse(JSON.stringify(this.completeDisclosureList));
                    this.loadingComplete();
                }
            }), (err) => {
                this.loadingComplete();
        });
    }

    /**
        Here the sorting is applied for the merged array inorder to get the list based on the decreasing
        order of updateTimeStamp. If any action performed on a particular disclosure or if any disclosure
        created then that one will comes first in the list.
    */
    private getDashboardList(): any {
        const DISCLOSURE_VIEWS = this.result.disclosureViews || [];
        const TRAVEL_DASHBOARD_VIEWS = this.result.travelDashboardViews || [];
        const OPA_DETAILS = this.result.opaDashboardDto || [];
        const MERGED_LIST = [...DISCLOSURE_VIEWS, ...TRAVEL_DASHBOARD_VIEWS, ...OPA_DETAILS];
        return MERGED_LIST.sort((a, b) => b.updateTimeStamp - a.updateTimeStamp);
    }

    private loadingComplete() {
        this.isLoading = false;
    }

    getDashboardBasedOnTab() {
        if(this.currentSelected.tab === 'DISCLOSURE_HISTORY') {
            this.getDisclosureHistory();
        } else {
            this.completeDisclosureList = [];
            this.$fetchDisclosures.next();
        }
    }

    resetAndFetchDisclosure() {
        this.searchText = '';
        this.completeDisclosureList = [];
        this.getDashboardBasedOnTab();
    }

    actionsOnPageChange(event: number) {
        if (this.dashboardRequestObject.currentPage != event) {
            this.dashboardRequestObject.currentPage = event;
            this.getArrayListForPagination();
        }
    }
   
    /**
        Arranges data in each page according to the Maximum number of data. By default the maximum number of data shows in a page is 20.
        This function is implemented on purpose as we are removing the pagination functionality from server side. This is
        because we won't be having a huge data in the dashboard in real time(production environment). Hence we dont want
        to make unnecessary api calls to the server everytime for fetching the data.
    */
    private getArrayListForPagination(): void {
        const [START_INDEX, END_INDEX] = [this.getStartIndex(), this.getEndIndex()];
        // const dummyList = JSON.parse(JSON.stringify(this.completeDisclosureList));
        this.completeDisclosureList = this.completeDisclosureListCopy.slice(START_INDEX, END_INDEX + 1);
    }

    /**
        If there is only one page, then the maximum number of data would be 20. so we need to arrange the data from [0 to 19].
        i.e., the starting point would always be 0 and ending point is always 19. Otherwise, for instance if the user clicks
        on 2nd page, the starting point would be (2-1) * 20 = 20
    */
    private getStartIndex(): number {
        if (this.dashboardRequestObject.currentPage == 1) { return 0; }
        return (this.dashboardRequestObject.currentPage - 1) * this.DATA_PER_PAGE;
    }

    /**
        If there is only one page, then the maximum number of data would be 20. so we need to arrange the data from [0 to 19].
        i.e., the starting point would always be 0 and ending point is always 19. Otherwise, for instance if the user clicks
        on 2nd page, the ending point would be (20 * 2) - 1  = 39
    */
    private getEndIndex(): number {
        if (this.dashboardRequestObject.currentPage == 1) { return this.DATA_PER_PAGE - 1; }
        return (this.DATA_PER_PAGE * this.dashboardRequestObject.currentPage) - 1;
    }

    loadDashboardCount() {
        this.userDisclosureService.getCOIDashboardCount(this.dashboardRequestObject).subscribe((res: any) => {
            this.dashboardCount = res;
            this.isShowFilterAndSearch = !!res?.inProgressDisclosureCount;
            this.setIsShowCreateFlag();
        });
    }

    setIsShowCreateFlag() {
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

    setSelectedModuleCode(moduleName, id, disclosure, noOfcount) {
        if (noOfcount > 0) {
            switch (moduleName) {
                case 'sfi':
                    this.selectedModuleCode = 8;
                    break;
                case 'proposal':
                    this.selectedModuleCode = 3;
                    break;
                case 'Awards':
                    this.selectedModuleCode = 1;
                    break;
                default:
                    this.selectedModuleCode = 0;
            }
            this.disclosures = disclosure;
            this.fcoiTypeCode = disclosure?.fcoiTypeCode;
            this.isShowCountModal = true;
            this.currentDisclosureId = id;
            this.currentDisclosureNumber = disclosure.disclosureNumber || disclosure.coiDisclosureNumber;
            this.disclosureType = moduleName;
            this.inputType = 'DISCLOSURE_TAB';
            this.disclosureSequenceStatusCode = disclosure.dispositionStatusCode;
            this.personId = disclosure.personId;
        }
    }

    setFilter(type = 'ALL') {
        this.currentSelected.filter = type;
        this.dashboardRequestObject.filterType = type;
        this.dashboardRequestObject.currentPage = 1;
        this.resetAndFetchDisclosure();
    }

    closeModalEvent(event) {
        if (!event) {
            this.isShowCountModal = event;
        }
    }

    redirectToDisclosure(disclosure: UserDisclosure) {
        let redirectUrl;
        if (disclosure.travelDisclosureId) {
            const isTravelDisclosureEditPage = ['1', '4', '5'].includes(disclosure.reviewStatusCode);
            redirectUrl = isTravelDisclosureEditPage ? CREATE_TRAVEL_DISCLOSURE_ROUTE_URL : POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL;
        } else if (disclosure.opaDisclosureId) {
            redirectUrl = OPA_REDIRECT_URL;
        } else {
            const isDisclosureEditPage = ['1', '5', '6'].includes(disclosure.reviewStatusCode);
            redirectUrl = isDisclosureEditPage ? CREATE_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL;
        }
        this._router.navigate([redirectUrl],
            { queryParams: { disclosureId: disclosure.travelDisclosureId || disclosure.coiDisclosureId || disclosure.opaDisclosureId} });
    }

    getColorBadges(disclosure: UserDisclosure) {
        if (disclosure?.travelDisclosureId) {
            return 'bg-travel-clip';
        }
        if (disclosure?.opaDisclosureId) {
            return 'bg-opa-clip';
        }
        switch (disclosure.fcoiTypeCode) {
            case '1':
                return 'bg-fcoi-clip';
            case '2':
                return 'bg-proposal-clip';
            case '3':
                return 'bg-award-clip';
            default:
                return;
        }
    }

    modalHeader(disclosure: UserDisclosure) {
        if (!disclosure.opaDisclosureId && (disclosure.fcoiTypeCode === '2' || disclosure.fcoiTypeCode === '3')) {
            if (disclosure.fcoiTypeCode === '2') {
                return `#${disclosure.proposalId} - ${disclosure.proposalTitle}`;
            } else if (disclosure.fcoiTypeCode === '3') {
                return `#${disclosure.awardId} - ${disclosure.awardTitle}`;
            }
        } else {
            return `#${disclosure.opaDisclosureId}`;
        }
    }

    formatTravellerTypes(travellerTypes: string): string {
        return travellerTypes ? (travellerTypes.split(',').map(travellerType => travellerType.trim()).join(', ')) : '';
    }

    getSearchPlaceHolder() {
        if (this.currentSelected.tab !== 'TRAVEL_DISCLOSURES') {
            return 'Search by Project Title, Disclosure Status, Disposition Status, Review Status, Department Name';
        } else {
            return 'Search by Entity Name, Department Name, Traveller Type, Destination, Review Status, Document Status, Purpose';
        }
    }

    getDisclosureHistory() {
        this.isLoading = true;
        this.completeDisclosureList =  [];
        this.$subscriptions.push(this.userDisclosureService.getDisclosureHistory({'filterType':this.currentSelected.filter}).subscribe((data: any) => {
            this.completeDisclosureList =  this.getAllDisclosureHistories(data);;
            this.loadingComplete();
        }));
    }

    getAllDisclosureHistories(data: any): any {
        const DISCLOSURE_HISTORY = data.disclosureHistoryDtos || [];
        const OPA_HISTORY = data.opaDashboardDtos || [];
        return [...DISCLOSURE_HISTORY, ...OPA_HISTORY];
    }

    openFCOIModal(type) {
        this.headerService.$openModal.next(type);
    }

    viewSlider(event) {
        this.showSlider = event.flag;
        this.entityId = event.entityId;
        setTimeout(() => {
            openSlider('disclosure-entity-view-2-slider');
        });
    }

    validateSliderClose() {
        closeSlider('disclosure-entity-view-2-slider');
        setTimeout(() => {
            this.showSlider = false;
		}, 500);
	}

    getActiveFCOI() {
        this.fcoiDatesRemaining();
        return this.headerService.activeDisclosures.filter(disclosure =>
            disclosure?.fcoiTypeCode === '1' );
    }

    fcoiDatesRemaining() {
        this.headerService.activeDisclosures.forEach(disclosure => {
            if (disclosure?.fcoiTypeCode === '1' && disclosure?.versionStatus == 'PENDING') {
                this.hasPendingFCOI = true;
            }
            if (disclosure?.fcoiTypeCode === '1' && disclosure?.versionStatus !== 'PENDING') {
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
            disclosure?.fcoiTypeCode === '1' && disclosure?.versionStatus !== 'PENDING');
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
        this.$subscriptions.push(this.userDisclosureService.createOPA(this.commonService.getCurrentUserDetail('personId'),
            this.commonService.getCurrentUserDetail('homeUnit'))
            .subscribe((res: any) => {
                this._router.navigate(['/coi/opa/form'], {queryParams: {disclosureId: res.opaDisclosureId}});
            }));
    }

}
