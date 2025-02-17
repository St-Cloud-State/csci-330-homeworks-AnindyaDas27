// Iterative QuickSort implementation using C++ STL stack
#include <iostream>
#include <stack>
#include <vector>
#include <algorithm> // for std::swap

// Partition function (Lomuto partition scheme)
// This function rearranges the array elements such that elements less than the pivot
// are on the left and elements greater than the pivot are on the right.
int partition(std::vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Choosing the last element as pivot
    int i = low - 1;       // Index for smaller elements

    for (int j = low; j < high; ++j) {
        if (arr[j] < pivot) { // If current element is smaller than pivot
            ++i;
            std::swap(arr[i], arr[j]);
        }
    }
    std::swap(arr[i + 1], arr[high]); // Place pivot at its correct position
    return i + 1;
}

// Iterative QuickSort using stack
void quickSortIterative(std::vector<int>& arr) {
    std::stack<std::pair<int, int>> stack; // Stack to store array index ranges

    stack.push({0, arr.size() - 1}); // Push the entire range of the array

    while (!stack.empty()) {
        int low = stack.top().first;
        int high = stack.top().second;
        stack.pop();

        if (low < high) {
            int pivotIndex = partition(arr, low, high);

            // Push left side to stack if it has more than one element
            if (pivotIndex - 1 > low) {
                stack.push({low, pivotIndex - 1});
            }
            // Push right side to stack if it has more than one element
            if (pivotIndex + 1 < high) {
                stack.push({pivotIndex + 1, high});
            }
        }
    }
}

int main() {
    std::vector<int> arr = {34, 7, 23, 32, 5, 62}; // Input array

    std::cout << "Original array: ";
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    quickSortIterative(arr); // Call the iterative QuickSort function

    std::cout << "Sorted array: ";
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    return 0;
}