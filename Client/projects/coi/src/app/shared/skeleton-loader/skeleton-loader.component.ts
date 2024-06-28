import { Component, EventEmitter, Input, Output } from '@angular/core';
import { animate, group, style, transition, trigger } from '@angular/animations';

type LoaderType = 'PROFILE_CARD' | 'SQUARE' | 'RECTANGLE' | 'CIRCLE';
type TableLoaderType = { rows: number, columns: number };

@Component({
    selector: 'app-skeleton-loader',
    templateUrl: './skeleton-loader.component.html',
    styleUrls: ['./skeleton-loader.component.scss'],
    animations: [
        trigger('heightAnimationOnLeave', [
            transition(':leave', [
                style({ height: '*', opacity: 1, 'padding-top': '*', 'padding-bottom': '*', overflow: 'hidden' }),
                group([
                    animate('300ms', style({ height: '0' })),
                    animate('300ms ease-in-out', style({ opacity: 0, 'padding-top': '0', 'padding-bottom': '0' }))
                ])
            ])
        ])
    ]
})
export class SkeletonLoaderComponent {

    @Input() uniqueId: string = 'custom';
    @Input() retryBtnName: string = 'Try again';
    @Input() loaderType: LoaderType | TableLoaderType; // if table give rows and columns
    @Input() viewMode: 'LOADER' | 'RETRY' | 'NONE' = 'LOADER';

    @Output() emitRetry: EventEmitter<any> = new EventEmitter();

    retry(): void {
        this.viewMode = 'LOADER';
        this.emitRetry.emit(this.viewMode);
    }

    generateArray(length: number): any[] {
        return Array.from({ length });
    }
}
