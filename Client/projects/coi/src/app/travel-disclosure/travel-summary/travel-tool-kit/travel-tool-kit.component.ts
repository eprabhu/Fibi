import { Component } from '@angular/core';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-travel-tool-kit',
    templateUrl: './travel-tool-kit.component.html',
    styleUrls: ['./travel-tool-kit.component.scss'],
    animations: [slideHorizontal]
})
export class TravelToolKitComponent {

    jumpToSection(section) {
        const sectionHeight = document.getElementById(section).offsetTop - 270;
        document.getElementById('COI_SCROLL').scrollTo({ top: sectionHeight, behavior: 'smooth' });
    }


}
