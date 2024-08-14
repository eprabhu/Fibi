import { Component, ElementRef, ViewChild } from '@angular/core';
import { openCoiSlider } from '../../common/utilities/custom-utilities';
import { Router } from '@angular/router';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityDataStoreService } from '../entity-data-store.service';
import { EntityDetails } from '../shared/entity-interface';

@Component({
  selector: 'app-header-details',
  templateUrl: './header-details.component.html',
  styleUrls: ['./header-details.component.scss']
})
export class HeaderDetailsComponent {

    @ViewChild('mainEntityHeaders', { static: true }) mainEntityHeaders: ElementRef;

    sliderElementId: any;
    isShowOptions = false;
    showSlider = false;
    isShowNavBarOverlay = false;
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    entityFullAddress: string = '';

    constructor(public router: Router, public dataStore: EntityDataStoreService) {}
    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    viewSlider(event) {
        this.showSlider = event;
        this.sliderElementId = 'duns-match-slider';
        setTimeout(() => {
            openCoiSlider(this.sliderElementId);
        });
    }

    validateSliderClose() {
        setTimeout(() => {
            this.showSlider = false;
            this.sliderElementId = '';
		}, 500);
	  }

    onClickMenuBar() {
        const NAV_ELEMENT = document.getElementById('responsive-nav');
        const IS_MENU_SHOW = NAV_ELEMENT.classList.contains('show-menu');
        const IS_SCREEN = window.innerWidth <= 1300;

        if (IS_MENU_SHOW) {
            NAV_ELEMENT.classList.remove('show-menu');
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = false;
            }
        } else {
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = true;
            }
            NAV_ELEMENT.classList.toggle('show-menu', IS_SCREEN);
        }
    }

    private getDataFromStore() {
        const entityData = this.dataStore.getData();
        if (!entityData || isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData.entityDetails;
        this.getEntityFullAddress();
    }

    getEntityFullAddress() {
        let address = this.entityDetails?.primaryAddressLine1;
        if (this.entityDetails?.primaryAddressLine2) {
            address = address + ' , ' + this.entityDetails?.primaryAddressLine2;
        }
        if(this.entityDetails?.city) {
            address = address + ' , ' + this.entityDetails?.city;
        }
        if(this.entityDetails?.state) {
            address = address + ' , ' + this.entityDetails?.state;
        }
        if(this.entityDetails?.country?.countryName) {
            address = address + ' , ' + this.entityDetails?.country?.countryName;
        }
        this.entityFullAddress = address;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }
}
