from datamodel import List

# Base solution
class Solution:
    def shuffle(self, nums: List[int], n: int) -> List[int]:
        shuffled = []
        for i in range(n):
            shuffled.append(nums[i])
            shuffled.append(nums[i + n])
        return shuffled
    
# Optimized solution using list comprehension
class Solution:
    def shuffle(self, nums: List[int], n: int) -> List[int]:
        return [nums[i // 2 + (i % 2) * n] for i in range(2 * n)]