import { Component } from '@angular/core';

@Component({
  selector: 'app-entity-history',
  templateUrl: './entity-history.component.html',
  styleUrls: ['./entity-history.component.scss']
})
export class EntityHistoryComponent {

    ngOnInit() {
        window.scrollTo(0,0);
    }
}
