import { Component, OnInit } from '@angular/core';
import { UserDisclosureService } from './user-disclosure.service';
import { UserDashboardService } from '../user-dashboard.service';
import { CommonService } from '../../common/services/common.service';
import { CREATE_DISCLOSURE_ROUTE_URL, CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, POST_CREATE_DISCLOSURE_ROUTE_URL } from '../../app-constants';
import { Router } from '@angular/router';
import { ActiveDisclosure , UserDisclosure } from './user-disclosure-interface';
import { Subject, interval } from 'rxjs';
import { debounce, switchMap } from 'rxjs/operators';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
@Component({
    selector: 'app-user-disclosure',
    templateUrl: './user-disclosure.component.html',
    styleUrls: ['./user-disclosure.component.scss']
})

export class UserDisclosureComponent implements OnInit {
    isShowCountModal = false;
    searchText = '';
    currentSelected = {
        tab: 'IN_PROGRESS_DISCLOSURES',
        filter: 'ALL',
    };
    dashboardRequestObject = {
        advancedSearch: 'L',
        pageNumber: 20,
        sort: {},
        tabName: 'IN_PROGRESS_DISCLOSURES',
        isDownload: false,
        filterType: 'ALL',
        currentPage: '1',
        property2: ''
    };
    filteredDisclosureArray: UserDisclosure[] = [];
    dashboardCount: any;
    isActiveDisclosureAvailable = false;
    selectedModuleCode: number;
    currentDisclosureId: any;
    currentDisclosureNumber: any;
    disclosureType: any;
    inputType: string;
    coiList: [];
    ishover: [] = [];
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
    isShowNoDataCard = false;
    readMoreOrLess = [];

    constructor(public userDisclosureService: UserDisclosureService,
        public userDashboardService: UserDashboardService,
        public commonService: CommonService,
        private _router: Router) {
    }

    ngOnInit() {
        this.loadDashboard();
        this.$fetchDisclosures.next();
        this.loadDashboardCount();
        this.getSearchList();
    }

    loadDashboard() {
        this.isShowNoDataCard = false;
        this.$subscriptions.push(this.$fetchDisclosures.pipe(
            switchMap(() => this.userDisclosureService.getCOIDashboard(this.dashboardRequestObject))).subscribe((res: any) => {
            this.result = res;
            if (this.result) {
                this.isShowNoDataCard = true;
                this.filteredDisclosureArray =  res.disclosureViews || [];
            }
        }));
    }

    getDisclosures() {
        this.dashboardRequestObject.currentPage = '1';
        this.$debounceEventForDisclosureList.next();
    }

    getSearchList() {
        this.$subscriptions.push(this.$debounceEventForDisclosureList.pipe(debounce(() => interval(800))).subscribe((data: any) => {
        this.dashboardRequestObject.property2 = this.searchText;
          this.$fetchDisclosures.next();
        }
        ));
      }

    resetAndFetchDisclosure() {
        this.searchText = '';
        this.dashboardRequestObject.property2 = '';
        this.$fetchDisclosures.next();
    }

    actionsOnPageChange(event) {
        this.dashboardRequestObject.currentPage = event;
        this.$fetchDisclosures.next();
    }

    loadDashboardCount() {
        this.userDisclosureService.getCOIDashboardCount(this.dashboardRequestObject).subscribe((res: any) => {
            this.dashboardCount = res;
        });
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

    setTab(tabName) {
        this.currentSelected.tab = tabName;
        this.dashboardRequestObject.tabName = tabName;
        this.dashboardRequestObject.currentPage = '1';
        this.dashboardRequestObject.filterType = 'ALL';
        this.currentSelected.filter = 'ALL';
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
        this.dashboardRequestObject.currentPage = '1';
        this.resetAndFetchDisclosure();
    }

    closeModalEvent(event) {
        if (!event) {
            this.isShowCountModal = event;
        }
    }

    redirectToDisclosure(disclosure: UserDisclosure) {
        const redirectUrl = disclosure.travelDisclosureId ? CREATE_TRAVEL_DISCLOSURE_ROUTE_URL : (disclosure.reviewStatusCode === '1' ?
            CREATE_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL);
        this._router.navigate([redirectUrl],
            { queryParams: { disclosureId: disclosure.travelDisclosureId || disclosure.coiDisclosureId } });
    }

    getColorBadges(disclosure: UserDisclosure) {
        if (disclosure?.travelDisclosureId) {
            return 'bg-travel-clip';
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
        if (disclosure.fcoiTypeCode === '2' || disclosure.fcoiTypeCode === '3') {
            if (disclosure.fcoiTypeCode === '2') {
                return `#${disclosure.proposalId} - ${disclosure.proposalTitle}`;
            } else if (disclosure.fcoiTypeCode === '3') {
                return `#${disclosure.awardId} - ${disclosure.awardTitle}`;
            }
        }
    }

    formatTravellerTypes(travellerTypes: string): string {
        return travellerTypes ? (travellerTypes.split(',').map(travellerType => travellerType.trim()).join(', ')) : '';
    }

    getSearchPlaceHolder() {
        if(this.currentSelected.tab != 'TRAVEL_DISCLOSURES') {
            return 'Search by #Disclosure Number, Disclosure Id, Project Title, Disclosure Status, Disposition Status, Review Status, Department Name';
        } else {
            return 'Search by #Travel Disclosure Id, Entity Name, Department Name, Traveller Type, Destination, Review Status, Document Status, Purpose';
        }
    }

}
