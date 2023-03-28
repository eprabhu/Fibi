import { Component } from '@angular/core';
import { SfiService } from './sfi.service';

@Component({
    selector: 'app-sfi',
    templateUrl: './sfi.component.html',
    styleUrls: ['./sfi.component.scss']
})
export class SfiComponent {

    constructor(public sfiService: SfiService) { }

    ngOnInit() {
    }

}
